#pragma once

#include <stdio.h>
#include <signal.h>
#include <assert.h>
#include <functional>
#include <mutex>
#include <atomic>
#include <thread>
#include <signal.h>
#include <semaphore.h>
#include "ProcIndication.h"
#include "ProcRequest.h"
#include "GeneratedTypes.h"
#include "print_buff.h"
#include "PerfStats.h"
#include "htif_riscy.h"
#include "spike/disasm.h"
#include "spike/sim.h"
#include "spike/htif.h"
#include "spike/processor.h"
#include "spike/encoding.h"
#include "fesvr/term.h"

class ProcIndication : public ProcIndicationWrapper {
protected:
    const uint32_t core_num;

    // done & result
    int result;
    bool done;

    // tandem verify
    sim_t *sim;
    htif_isasim_t *spike_htif;
    disassembler_t *disassembler;
    int count; // inst count
    // keeping track of errors
    bool error_found;
    size_t lines_past_error; // # of lines to capture past first error
    size_t error_density; // # of errors found in region past current error
    // skipping verification packets
    // setting verification_packets_skipped to -1 causes the processor
    // to disable all verification and stdin synchronization
    uint64_t verification_packets_skipped;
    uint64_t verification_packets_printed;
    bool synchronization_packets_sent;

    // logging
    FILE *debug_file;
    PrintBuffer *print_buff;

    std::mutex proc_mu; // protect all above: result, tandem verify, logging
    sem_t sem; // notify done signals

    // htif (it has its own lock)
    htif_riscy_t *riscy_htif;

    // performance
    PerfStats *perf_stats;
    
    // handle ctrl-C
    sighandler_t handle_signal;

    // inner class to handle to host msg
    class ToHostHandler {
    private:
        // shared Q of msg
        struct ToHostMsg {
            uint32_t core;
            uint64_t v;
        };
        std::queue<ToHostMsg> to_host_msg_q;
        std::mutex msg_q_mu;
        std::atomic<int64_t> msg_q_size;

        std::thread handler_thread;
        void handler();

        ProcIndication *proc_ind; // ptr to outer class

    public:
        ToHostHandler(ProcIndication *p_ind);
        ~ToHostHandler();

        void enq_to_host_msg(uint32_t core, uint64_t v);
        void spawn_handler_thread();
    };
    ToHostHandler to_host_handler;

    // memory size
    const size_t mem_sz;
    // htif arguments
    std::vector<std::string> *htif_args;

    // shell cmd to be sent to processor
    char *shell_cmd;
    int cmd_delay_sec;

public:
    ProcIndication(
        unsigned int id,
        uint32_t core_cnt,
        FILE *dbg_f,
        PrintBuffer *buff,
        PerfStats *perf,
        sighandler_t sig_hdl,
        uint64_t verify_packets_skipped,
        uint64_t verify_packets_printed,
        bool sync_packets_sent,
        size_t mem_size,
        char *cmd,
        int cmd_delay
    ) : ProcIndicationWrapper(id),
        core_num(core_cnt),
        result(-1),
        done(false),
        sim(0),
        spike_htif(0),
        disassembler(0),
        count(0),
        error_found(false),
        lines_past_error(PrintBuffer::output_buff_lines / 2),
        error_density(0),
        verification_packets_skipped(verify_packets_skipped),
        verification_packets_printed(verify_packets_printed),
        synchronization_packets_sent(sync_packets_sent),
        debug_file(dbg_f),
        print_buff(buff),
        riscy_htif(0),
        perf_stats(perf),
        handle_signal(sig_hdl),
        to_host_handler(this),
        mem_sz(mem_size),
        htif_args(0),
        shell_cmd(cmd),
        cmd_delay_sec(cmd_delay)
    {
        std::lock_guard<std::mutex> lock(proc_mu);
        sem_init(&sem, 0, 0);
        // initialize riscv disassembler
        disassembler = new disassembler_t();
    }

    virtual ~ProcIndication();

    void set_riscy_htif(htif_riscy_t *p) { riscy_htif = p; }
    void set_htif_args(std::vector<std::string> *p) { htif_args = p; }
    virtual void spawn_to_host_handler() {
        to_host_handler.spawn_handler_thread();
    }

    int waitResult();
    void waitReset();
    virtual void resetDone();
    //virtual void debug_pc(uint64_t pc);
    //virtual void debug_excep(uint64_t ex);
    virtual void debug_verify(uint8_t core, VerificationPacket packet);
    virtual void to_host(uint8_t core, uint64_t v);
    virtual void perfResp(uint8_t core, ProcPerfResp resp);
    virtual void terminate(uint8_t core);
};

class ProcIndication_NoTandem : public ProcIndication {
public:
    virtual void debug_verify(uint8_t core, VerificationPacket packet);

    ProcIndication_NoTandem(
        unsigned int id,
        uint32_t core_cnt,
        FILE *dbg_f,
        PrintBuffer *buff,
        PerfStats *perf,
        sighandler_t sig_hdl,
        uint64_t verify_packets_skipped,
        uint64_t verify_packets_printed,
        bool sync_packets_sent,
        size_t mem_size,
        char *cmd,
        int cmd_delay
    ) : ProcIndication(id, core_cnt, dbg_f, buff, perf, sig_hdl, verify_packets_skipped,
                       verify_packets_printed, sync_packets_sent, mem_size, cmd, cmd_delay) {}
};

class ProcIndicationAssembly : public ProcIndication {
public:
    virtual void to_host(uint8_t core, uint64_t v);
    virtual void spawn_to_host_handler() {} // don't need this handler thread

    ProcIndicationAssembly(
        unsigned int id,
        uint32_t core_cnt,
        FILE *dbg_f,
        PrintBuffer *buff,
        PerfStats *perf,
        sighandler_t sig_hdl,
        uint64_t verify_packets_skipped,
        uint64_t verify_packets_printed,
        bool sync_packets_sent,
        size_t mem_size,
        char *cmd,
        int cmd_delay
    ) : ProcIndication(id, core_cnt, dbg_f, buff, perf, sig_hdl, verify_packets_skipped,
                       verify_packets_printed, sync_packets_sent, mem_size, cmd, cmd_delay) {}
};

