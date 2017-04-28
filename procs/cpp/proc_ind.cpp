#include <stdlib.h>
#include "proc_ind.h"

// ProcIndication
ProcIndication::~ProcIndication() {
    std::lock_guard<std::mutex> lock(proc_mu);
    if (sim) {
        delete sim;
        sim = 0;
        spike_htif = 0;
    }
    delete disassembler;
}

int ProcIndication::waitResult() {
    // ptr to send shell cmd
    int cmd_ptr = 0;
    // delay loops for cmd (each loop takes about 1ms)
    int cmd_delay_loops = cmd_delay_sec * 1000;

    while (sem_trywait(&sem) != 0) {
        {
            std::lock_guard<std::mutex> lock(proc_mu);
            // read stdin if necessary when spike_htif == 0
            // we need to lock htif, since to_host_handler may also access htif
            // FIXME feed stdin to riscy_htif will send req to FPGA, hopefully this won't deadlock...
            riscy_htif->lock();
            if (riscy_htif->bcd_wait_for_stdin() && (spike_htif == 0 || spike_htif->bcd_wait_for_stdin())) {
                int ch = -1; // the character to be sent to processor, -1 means not sending anything
                // get the character to send
                if(shell_cmd) {
                    // read from stored shell cmd
                    if(cmd_delay_loops > 0) {
                        // still waiting for linux to boot...
                        cmd_delay_loops--;
                    } else {
                        // send one character from the shell cmd
                        if(shell_cmd[cmd_ptr] != 0) {
                            ch = shell_cmd[cmd_ptr];
                            cmd_ptr++;
                        } else {
                            // all chars of the cmd is sent, now send a '\n' to execute the cmd
                            // we are done with stored shell cmd
                            ch = '\n';
                            shell_cmd = 0;
                        }
                    }
                } else {
                    // read from keyboard
                    ch = canonical_terminal_t::read();
                }
                // send character to processor via htif
                if(ch != -1) {
                    riscy_htif->bcd_feed_stdin(ch);
                    if (spike_htif != 0) {
                        spike_htif->bcd_feed_stdin(ch);
                    }
                }
            }
            riscy_htif->unlock();
        }
        usleep(1000); // delay 1ms
    }
    return result;
}

void ProcIndication::waitReset() {
    sem_wait(&sem);
}

void ProcIndication::resetDone() {
    {
        std::lock_guard<std::mutex> lock(proc_mu);
        fprintf(stderr, "resetDone\n");
        done = false;
        // "reset" spike too
        if (sim != NULL) {
            delete sim;
            sim = 0;
            spike_htif = 0; // reset to prevent segfault
        }
    }
    sem_post(&sem);
}

//void ProcIndication::debug_pc(uint64_t pc) {
//    //fprintf(debug_file, "PC: %p\n", (void*)pc);
//}
//
//void ProcIndication::debug_excep(uint64_t ex) {
//    fprintf(debug_file, "Mem1: Load Access Exception: %p\n", (void*)ex);
//}

void ProcIndication::debug_verify(uint8_t core, VerificationPacket packet) {
    // FIXME only do core 0 for singlethreaded programs
    if(core != 0) return;

    std::lock_guard<std::mutex> lock(proc_mu); // lock the whole thing

    // pc, addr, data1, data2, instruction, fpdata
    if (!done) {
        bool first_instruction = false;
        bool mismatch = false;
        bool warning = false;

        reg_t pc = 0;
        // initialize spike if it doesn't exist yet
        if (!sim) {
            first_instruction = true;
            sim = new sim_t("RV64IMAFD", 1, mem_sz >> 20, *htif_args); // TODO only 1 core now
            spike_htif = sim->get_htif();
            spike_htif->disable_stdout();
            // [sizhuo] register signal handler, override handler in sim
            signal(SIGINT, handle_signal);
        }

        // get pc
        if (packet.skipped_packets != 0) {
            // fast-forward if necessary
            // simulate for all skipped packets
            for (size_t i = 0 ; i < ((size_t) packet.skipped_packets) ; i++) {
                sim->single_step_synchronize(false, 0);
            }
            // fprintf(stderr, "skipped %d packets\n", (int) packet.skipped_packets);
            // update count for skipped packets
            // FIXME: This doesn't workd if verification_packets_skipped is too large (too large means >= 2*31)
            count += (int) packet.skipped_packets; 
            // get fast-forwarded pc
            processor_t *p = sim->get_core(0);
            pc = p->get_state()->pc;
        } else if (first_instruction) {
            pc = 0x200;
        } else {
            processor_t *p = sim->get_core(0);
            pc = p->get_state()->pc;
        }
        // check pc
        if (pc != packet.pc && pc == packet.addr) {
            // exception?
            // skip this for now because this is (probably) an extra packet from the processor
            // DEBUGGING
            print_buff->add_line(
                "[CRITICAL WARNING] Extra Packet:\n  [PROC]  %llx: (%08x) %-32s\n",
                (long long)packet.pc, packet.instruction,
                disassembler->disassemble(packet.instruction).c_str()
            );
            return;
        } else if (pc != packet.pc) {
            mismatch = true;
        }

        // get trap_string
        const char* null_string = "";
        const char* exception_string = " <exception>";
        const char* sw_interrupt_string = " <sw interrupt>";
        const char* timer_interrupt_string = " <timer interrupt>";
        const char* host_interrupt_string = " <host interrupt>";
        const char* unknown_interrupt_string = " <unknown interrupt>";
        const char* trap_string = null_string;
        if (packet.trap) {
            // This packet was an exception instead of executing an instruction
            if ((0x80 & packet.trap_type) == 0) {
                // exception
                trap_string = exception_string;
            } else {
                // interrupt
                if (packet.trap_type == 0x80) {
                    // Software interrupt
                    trap_string = sw_interrupt_string;
                } else if (packet.trap_type == 0x81) {
                    // Timer interrupt
                    trap_string = timer_interrupt_string;
                } else if (packet.trap_type == 0x82) {
                    // Host interrupt
                    trap_string = host_interrupt_string;
                } else {
                    trap_string = unknown_interrupt_string;
                }
            }
        }

        // force specific non-deterministic behavior
        bool force_trap = false;
        uint64_t force_trap_cause = 0;
        // --- reading some CSRs
        bool is_csr_read = ((packet.instruction & 0x7f) == 0x73) && (((packet.instruction >> 12) & 0x3) != 0);
        uint64_t csr_data = packet.data1;
        if (is_csr_read) {
            int csr = (packet.instruction >> 20) & 0xFFF;
            processor_t *p = sim->get_core(0);
            switch (csr) {
                // defines from riscv-isa-sim/riscv/encoding.h
                case CSR_MTIME:
                case CSR_STIME:
                case CSR_STIMEW:
                    sim->rtc = csr_data;
                    break;
                case CSR_TIME:
                case CSR_TIMEW:
                    sim->rtc = csr_data;
                    p->get_state()->sutime_delta = 0;
                    break;
                case CSR_CYCLE:
                case CSR_CYCLEW:
                case CSR_INSTRET:
                case CSR_INSTRETW:
                    p->get_state()->minstret = csr_data;
                    p->get_state()->suinstret_delta = 0;
                    break;
                case CSR_MIP:
                    p->get_state()->mip = csr_data;
                    break;
                case CSR_MFROMHOST:
                    if (csr_data != 0) {
                        // if (p->fromhost_fifo.empty()) {
                        //     fprintf(stderr, "[ERROR] csr_data != 0, but p->fromhost_fifo.empty() is true\n");
                        //     throw(-1);
                        // }
                        // if (csr_data != p->fromhost_fifo.front()) {
                        //     fprintf(stderr, "[ERROR] csr_data != 0, but p->fromhost_fifo.front() doesn't match\n");
                        //     throw(-1);
                        // }
                        // p->fromhost_fifo.pop();
                        while (!p->fromhost_fifo.empty()) {
                            p->fromhost_fifo.pop();
                        }
                        p->get_state()->fromhost = csr_data;
                    } else {
                        p->get_state()->fromhost = 0;
                    }
                    break;
            }
        }
        // --- timer interrupt
        if (packet.trap && (packet.trap_type == 0x81)) {
            force_trap = true;
            force_trap_cause = ((1ULL) << 63) | 1;
        }
        // --- host interrupt
        if (packet.trap && (packet.trap_type == 0x82)) {
            force_trap = true;
            force_trap_cause = ((1ULL) << 63) | 2;
        }
        
        // do a step of the simulation
        sim->single_step_synchronize(force_trap, force_trap_cause);

        // get instruction from simulator
        uint32_t instr = 0;
        // use the mmu in the simulator
        mmu_t *mmu = sim->get_core(0)->get_mmu();
        try {
            instr = mmu->load_uint32(pc);
            if (instr != packet.instruction) {
                processor_t *p = sim->get_core(0);
                reg_t next_pc = p->get_state()->pc;
                if (next_pc != 0x100 && next_pc != 0x140 && next_pc != 0x180 && next_pc != 0x1c0) {
                    // Only throw a mismatch error if instruction didn't cause an exception.
                    // This fixes false mismatches when instruction access page faults
                    // return different instructions values.
                    mismatch = true;
                } else {
                    warning = true;
                }
            }
        } catch (trap_t& t) {
            // get instr from our processor
            instr = packet.instruction;
        }

        // get data
        uint64_t spike_data = 0;
        const char* dst_reg = "ILLEGAL_REG";
        const char* spike_reg = "ILLEGAL_REG";
        bool write_to_reg = false;
        if (packet.dst & 0x40) {
            // valid destination register
            write_to_reg = true;
            processor_t *p = sim->get_core(0);
            if (pc == packet.pc && instr == packet.instruction) {
                // spike and proc are on the same instruction
                if (packet.dst & 0x20) {
                    // fpu register
                    spike_data = p->get_state()->FPR[packet.dst & 0x1F];
                    dst_reg = fpr_name[packet.dst & 0x1F];
                    spike_reg = fpr_name[packet.dst & 0x1F];
                } else {
                    // gpr register
                    spike_data = p->get_state()->XPR[packet.dst & 0x1F];
                    dst_reg = xpr_name[packet.dst & 0x1F];
                    spike_reg = xpr_name[packet.dst & 0x1F];
                }
                if (spike_data != packet.data1) {
                    if (!packet.trap) {
                        // don't worry about data mismatches for spike
                        mismatch = true;
                    }
                }
            } else {
                // spike and proc are on different instructions
                if (packet.dst & 0x20) {
                    // fpu register
                    dst_reg = fpr_name[packet.dst & 0x1F];
                } else {
                    // gpr register
                    dst_reg = xpr_name[packet.dst & 0x1F];
                }
                // assume spike is using gpr
                spike_reg = xpr_name[(instr >> 7) & 0x1F];
                processor_t *p = sim->get_core(0);
                spike_data = p->get_state()->XPR[(instr >> 7) & 0x1F];
            }
        } else {
            // lets not assume that spike isn't writing to a register
            // but lets assume its using a GPR
            spike_reg = xpr_name[(instr >> 7) & 0x1F];
            processor_t *p = sim->get_core(0);
            spike_data = p->get_state()->XPR[(instr >> 7) & 0x1F];
        }

        // printing information
        int curcount = count++;
        if (!mismatch && !warning) {
            if (write_to_reg) {
                print_buff->add_line(
                    "(%08d) %llx: (%08x) %-32s [%s = %08llx] %s\n",
                    curcount, (long long)packet.pc, packet.instruction,
                    disassembler->disassemble(packet.instruction).c_str(), dst_reg, (long long)packet.data1,
                    trap_string);
            } else {
                print_buff->add_line(
                    "(%08d) %llx: (%08x) %-32s %s\n",
                    curcount, (long long)packet.pc, packet.instruction,
                    disassembler->disassemble(packet.instruction).c_str(),
                    trap_string);
            }
        } else if (warning) {
            if (!write_to_reg && (packet.pc != pc || packet.instruction != instr)) {
                // proc is not writing to a register, but lets assume that spike is so we don't miss anything
                print_buff->add_line(
                    "[WARNING] Processor may have diverged from Spike simulator:\n"
                    "  [PROC]  (%08d) %llx: (%08x) %-32s%s\n"
                    "  [SPIKE] (%08d) %llx: (%08x) %-32s [%s = %08llx]\n",
                    curcount, (long long)packet.pc, packet.instruction,
                    disassembler->disassemble(packet.instruction).c_str(),
                    trap_string,
                    curcount, (long long)pc, instr,
                    disassembler->disassemble(instr).c_str(), spike_reg, (long long)spike_data);
            } else if (write_to_reg) {
                print_buff->add_line(
                    "[WARNING] Processor may have diverged from Spike simulator:\n"
                    "  [PROC]  (%08d) %llx: (%08x) %-32s [%s = %08llx]%s\n"
                    "  [SPIKE] (%08d) %llx: (%08x) %-32s [%s = %08llx]\n",
                    curcount, (long long)packet.pc, packet.instruction,
                    disassembler->disassemble(packet.instruction).c_str(), dst_reg, (long long)packet.data1,
                    trap_string,
                    curcount, (long long)pc, instr,
                    disassembler->disassemble(instr).c_str(), spike_reg, (long long)spike_data);
            } else {
                print_buff->add_line(
                    "[WARNING] Processor may have diverged from Spike simulator:\n"
                    "  [PROC]  (%08d) %llx: (%08x) %-32s%s\n"
                    "  [SPIKE] (%08d) %llx: (%08x) %-32s\n",
                    curcount, (long long)packet.pc, packet.instruction,
                    disassembler->disassemble(packet.instruction).c_str(),
                    trap_string,
                    curcount, (long long)pc, instr,
                    disassembler->disassemble(instr).c_str());
            }
        } else {
            // mismatch
            if (!error_found) {
                // there was a mismatch, start countdown until exiting the program.
                error_found = true;
                error_density = 0;
                lines_past_error = PrintBuffer::output_buff_lines / 2;
            } else {
                // there has already been an error recently
            }
            error_density++;
            if (!write_to_reg && (packet.pc != pc || packet.instruction != instr)) {
                // proc is not writing to a register, but lets assume that spike is so we don't miss anything
                print_buff->add_line(
                    "[CRITICAL WARNING] Processor diverged from Spike simulator:\n"
                    "  [PROC]  (%08d) %llx: (%08x) %-32s%s\n"
                    "  [SPIKE] (%08d) %llx: (%08x) %-32s [%s = %08llx]\n",
                    curcount, (long long)packet.pc, packet.instruction,
                    disassembler->disassemble(packet.instruction).c_str(),
                    trap_string,
                    curcount, (long long)pc, instr,
                    disassembler->disassemble(instr).c_str(), spike_reg, (long long)spike_data);
            } else if (write_to_reg) {
                print_buff->add_line(
                    "[CRITICAL WARNING] Processor diverged from Spike simulator:\n"
                    "  [PROC]  (%08d) %llx: (%08x) %-32s [%s = %08llx]%s\n"
                    "  [SPIKE] (%08d) %llx: (%08x) %-32s [%s = %08llx]\n",
                    curcount, (long long)packet.pc, packet.instruction,
                    disassembler->disassemble(packet.instruction).c_str(), dst_reg, (long long)packet.data1,
                    trap_string,
                    curcount, (long long)pc, instr,
                    disassembler->disassemble(instr).c_str(), spike_reg, (long long)spike_data);
            } else {
                print_buff->add_line(
                    "[CRITICAL WARNING] Processor diverged from Spike simulator:\n"
                    "  [PROC]  (%08d) %llx: (%08x) %-32s%s\n"
                    "  [SPIKE] (%08d) %llx: (%08x) %-32s\n",
                    curcount, (long long)packet.pc, packet.instruction,
                    disassembler->disassemble(packet.instruction).c_str(),
                    trap_string,
                    curcount, (long long)pc, instr,
                    disassembler->disassemble(instr).c_str());
            }
        }

        // countdown if necessary
        if (error_found) {
            if (lines_past_error == 0) {
                // done collecting extra lines
                print_buff->flush_print();
                if (error_density > PrintBuffer::output_buff_lines / 4) {
                    // exit if there have been too many errors
                    exit(1);
                } else {
                    // otherwise continue working
                    error_found = false;
                }
            } else {
                lines_past_error--;
            }
        }

        // XXX [sizhuo] this is already done in waitResult
        // read stdin if necessary
        //if (riscy_htif->bcd_wait_for_stdin() && spike_htif->bcd_wait_for_stdin()) {
        //    int ch = canonical_terminal_t::read();
        //    if(ch != -1) {
        //        // send to processor - this cals call_from_host
        //        riscy_htif->bcd_feed_stdin(ch);
        //        // send to spike & tick to write mfromhost
        //        if (spike_htif != 0) {
        //            spike_htif->bcd_feed_stdin(ch);
        //        }
        //    } else {
        //        // No action necessary, just keep running
        //    }
        //}
    }
}

void ProcIndication::to_host(uint8_t core, uint64_t v) {
    // debug
    fprintf(debug_file, "[to_host] core %d, value 0x%016llx\n", int(core), (long long) v);

    to_host_handler.enq_to_host_msg(core, v);
}

void ProcIndication::perfResp(uint8_t core, ProcPerfResp resp) {
    perf_stats->inform_resp(core, resp.loc, resp.pType, resp.data);
}

void ProcIndication::terminate(uint8_t core) {
    {
        std::lock_guard<std::mutex> lock(proc_mu);
        if (error_found) {
            // print output_buff
            print_buff->flush_print();
        }
        fprintf(stderr, "\nCore %d signaled terminate\n", (int)core);
        result = 0; // result default to 0 (success val)
        done = true;
    }
    sem_post(&sem); // notify program finish
}

// ProcIndication_NoTandem
void ProcIndication_NoTandem::debug_verify(uint8_t core, VerificationPacket packet) {
    // FIXME only do core 0 for singlethreaded programs
    if(core != 0) return;

    std::lock_guard<std::mutex> lock(proc_mu);

    // initialize spike if it doesn't exist yet
    if (!sim) {
        // First Packet
        sim = new sim_t("RV64IMAFD", 1, mem_sz >> 20, *htif_args); // TODO only 1 core now
        count = verification_packets_skipped;
    }

    const char* null_string = "";
    const char* exception_string = " <exception>";
    const char* sw_interrupt_string = " <sw interrupt>";
    const char* timer_interrupt_string = " <timer interrupt>";
    const char* host_interrupt_string = " <host interrupt>";
    const char* unknown_interrupt_string = " <unknown interrupt>";
    const char* trap_string = null_string;

    // check traps
    if (packet.trap) {
        // This packet was an exception instead of executing an instruction
        if ((0x80 & packet.trap_type) == 0) {
            // exception
            trap_string = exception_string;
        } else {
            // interrupt
            if (packet.trap_type == 0x80) {
                // Software interrupt
                trap_string = sw_interrupt_string;
            } else if (packet.trap_type == 0x81) {
                // Timer interrupt
                trap_string = timer_interrupt_string;
            } else if (packet.trap_type == 0x82) {
                // Host interrupt
                trap_string = host_interrupt_string;
            } else {
                trap_string = unknown_interrupt_string;
            }
        }
    }
    
    // get data
    const char* dst_reg = "ILLEGAL_REG";
    bool write_to_reg = false;
    if (packet.dst & 0x40) {
        // valid destination register
        write_to_reg = true;
        if (packet.dst & 0x20) {
            // fpu register
            dst_reg = fpr_name[packet.dst & 0x1F];
        } else {
            // gpr register
            dst_reg = xpr_name[packet.dst & 0x1F];
        }
    }

    // printing information
    if (write_to_reg) {
        printf( "(%08d) %llx: (%08x) %-32s [%s = %08llx] %s\n",
                count, (long long)packet.pc, packet.instruction,
                disassembler->disassemble(packet.instruction).c_str(), dst_reg, (long long)packet.data1,
                trap_string);
    } else {
        printf( "(%08d) %llx: (%08x) %-32s %s\n",
                count, (long long)packet.pc, packet.instruction,
                disassembler->disassemble(packet.instruction).c_str(),
                trap_string);
    }

    // increment count
    count++;

    if (verification_packets_printed == 1) {
        // All done!
        exit(0);
    } else if (verification_packets_printed != 0) {
        verification_packets_printed--;
    }
}

// ProcIndicationAssembly
void ProcIndicationAssembly::to_host(uint8_t core, uint64_t v) {
    if(core == 0) {
        {
            std::lock_guard<std::mutex> lock(proc_mu);
            if (error_found) {
                // print output_buff
                print_buff->flush_print();
            }
            // don't bother passing it to the htif
            // the only thing that goes in to_host are exit codes
            if (v == 1) {
                fprintf(stderr, "[32;1mPASSED[0m\n");
            } else {
                fprintf(stderr, "[31;1mFAILED %lld[0m\n", (long long)(v >> 1));
            }
            result = v >> 1;
            done = true;
        }
        sem_post(&sem);
    }
    else {
        fprintf(stderr, "[ProcIndicationAssembly] recv exit code %016llx from core %d\n",
                (long long unsigned)v, int(core));
    }
}

