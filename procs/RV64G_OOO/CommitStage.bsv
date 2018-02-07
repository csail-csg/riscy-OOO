
// Copyright (c) 2017 Massachusetts Institute of Technology
// 
// Permission is hereby granted, free of charge, to any person
// obtaining a copy of this software and associated documentation
// files (the "Software"), to deal in the Software without
// restriction, including without limitation the rights to use, copy,
// modify, merge, publish, distribute, sublicense, and/or sell copies
// of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
// 
// The above copyright notice and this permission notice shall be
// included in all copies or substantial portions of the Software.
// 
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
// EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
// MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
// NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
// BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
// ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
// CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.

`include "ProcConfig.bsv"
import Vector::*;
import GetPut::*;
import Cntrs::*;
import ConfigReg::*;
import FIFO::*;
import Types::*;
import ProcTypes::*;
import CCTypes::*;
import Performance::*;
import ReorderBuffer::*;
import ReorderBufferSynth::*;
import RenamingTable::*;
import CsrFile::*;
import StoreBuffer::*;
import VerificationPacket::*;
import RenameDebugIF::*;

typedef struct {
    // info about the inst blocking at ROB head
    Addr pc;
    IType iType;
    Maybe#(Trap) trap;
    RobInstState state;
    SpecBits specBits;
    Maybe#(SpecTag) specTag;
    // store buffer
    Bool stbEmpty;
    // CSR info: previlige mode
    Bit#(2) prv;
    // htif
    Bool htifStall;
} CommitStuck deriving(Bits, Eq, FShow);

interface CommitInput;
    // func units
    interface ReorderBufferSynth robIfc;
    interface RegRenamingTable rtIfc;
    interface CsrFile csrfIfc;
    interface StoreBuffer stbIfc;
    // TLB has stopped processing now
    method Bool tlbNoPendingReq;
    // set flags
    method Action setFlushTlbs;
    method Action setUpdateVMInfo;
    method Action setFlushReservation;
    // redirect
    method Action redirect_action(Addr trap_pc, Maybe#(SpecTag) spec_tag, InstTag inst_tag);
    // record if we commit a CSR inst or interrupt
    method Action commitCsrInstOrInterrupt;
    // performance
    method Bool doStats;
    // deadlock check
    method Bool checkDeadlock;
endinterface

typedef struct {
    RenameError err;
    Addr pc;
    IType iType;
    Maybe#(Trap) trap;
    Maybe#(SpecTag) specTag;
    SpecBits specBits;
} RenameErrInfo deriving(Bits, Eq, FShow);

interface CommitStage;
    // verification packets
    method Action initVerify(Bool sendSyncPack, Bit#(64) skippedPack, Bit#(64) packToIgore);
    method ActionValue#(VerificationPacket) debug_verify;
    // performance
    method Data getPerf(ComStagePerfType t); 
    // deadlock check
    interface Get#(CommitStuck) commitInstStuck;
    interface Get#(CommitStuck) commitUserInstStuck;
    // rename debug
    method Action startRenameDebug;
    interface Get#(RenameErrInfo) renameErr;
endinterface

// we apply actions the end of commit rule
// use struct to record actions to be done
typedef struct {
    Addr pc;
    Addr addr;
    Trap trap;
    Maybe#(SpecTag) specTag;
    InstTag instTag;
} CommitTrap deriving(Bits, Eq, FShow);

typedef struct {
    IType iType; // for sret & mrts
    Addr nextPc;
    Maybe#(SpecTag) specTag;
    InstTag instTag;
} CommitRedirect deriving(Bits, Eq, FShow);

typedef union tagged {
    void Invalid;
    Tuple2#(CSR, Data) CsrInst; // csr index + data
    Bit#(5) FpuInst; // fflags
} CommitWrCsrf deriving(Bits, Eq, FShow);

module mkCommitStage#(CommitInput inIfc)(CommitStage);
    Bool verbose = True;

    // func units
    ReorderBufferSynth rob = inIfc.robIfc;
    RegRenamingTable regRenamingTable = inIfc.rtIfc;
    CsrFile csrf = inIfc.csrfIfc;
    StoreBuffer stb = inIfc.stbIfc;

    // verify packets
`ifdef VERIFICATION_PACKETS
    Reg#(Bool)  sendSynchronizationPackets <- mkReg(False);
    Reg#(Bit#(64)) skippedVerificationPackets <- mkReg(0);
    Reg#(Bit#(64)) verificationPacketsToIgnore <- mkReg(0);
    Fifo#(4, VerificationPacket) verifyFifo <- mkCFFifo;
`endif

    // commit stage performance counters
`ifdef PERF_COUNT
    // inst
    Count#(Data) instCnt <- mkCount(0);
    Count#(Data) userInstCnt <- mkCount(0);
    Count#(Data) supComUserCnt <- mkCount(0);
    // branch/jump inst
    Count#(Data) comBrCnt <- mkCount(0);
    Count#(Data) comJmpCnt <- mkCount(0);
    Count#(Data) comJrCnt <- mkCount(0);
    // exception related
    Count#(Data) comRedirectCnt <- mkCount(0);
    Count#(Data) trapCnt <- mkCount(0);
    Count#(Data) sretCnt <- mkCount(0);
    Count#(Data) mrtsCnt <- mkCount(0);
`endif

    // deadlock check
`ifdef CHECK_DEADLOCK
    // timer to check deadlock
    Reg#(DeadlockTimer) commitInstTimer <- mkReg(0);
    Reg#(DeadlockTimer) commitUserInstTimer <- mkReg(0);
    // FIFOs to output deadlock info
    FIFO#(CommitStuck) commitInstStuckQ <- mkFIFO1;
    FIFO#(CommitStuck) commitUserInstStuckQ <- mkFIFO1;
    // wires to indicate that deadlock is reported, so reset timers
    PulseWire commitInstStuckSent <- mkPulseWire;
    PulseWire commitUserInstStuckSent <- mkPulseWire;
    // wires to reset timers since processor is making progress
    PulseWire commitInst <- mkPulseWire;
    PulseWire commitUserInst <- mkPulseWire;

    function CommitStuck commitStuck;
        let x = rob.deqPort[0].deq_data;
        return CommitStuck {
            pc: x.pc,
            iType: x.iType,
            trap: x.trap,
            state: x.rob_inst_state,
            specBits: x.spec_bits,
            specTag: x.spec_tag,
            stbEmpty: stb.isEmpty,
            prv: csrf.decodeInfo.prv,
            htifStall: False
        };
    endfunction

    (* fire_when_enabled *)
    rule checkDeadlock_commitInst(inIfc.checkDeadlock && commitInstTimer == maxBound);
        commitInstStuckQ.enq(commitStuck);
        commitInstStuckSent.send;
    endrule

    (* fire_when_enabled *)
    rule checkDeadlock_commitUserInst(inIfc.checkDeadlock && commitUserInstTimer == maxBound);
        commitUserInstStuckQ.enq(commitStuck);
        commitUserInstStuckSent.send;
    endrule

    (* fire_when_enabled, no_implicit_conditions *)
    rule incrDeadlockTimer(inIfc.checkDeadlock);
        function DeadlockTimer getNextTimer(DeadlockTimer t);
            return t == maxBound ? maxBound : t + 1;
        endfunction
        commitInstTimer <= (commitInst || commitInstStuckSent) ? 0 : getNextTimer(commitInstTimer);
        commitUserInstTimer <= (commitUserInst || commitUserInstStuckSent) ? 0 : getNextTimer(commitUserInstTimer);
    endrule
`endif

    // rename debug
    Reg#(Bool) renameDebugStarted <- mkConfigReg(False);
    Reg#(Maybe#(RenameErrInfo)) renameErrInfo <- mkConfigReg(Invalid);
    Bool canSetRenameErr = renameDebugStarted && renameErrInfo == Invalid; // only set err info once
    // only send 1 error msg
    FIFO#(RenameErrInfo) renameErrQ <- mkFIFO1;

    rule sendRenameErr(renameDebugStarted &&& renameErrInfo matches tagged Valid .info);
        renameErrQ.enq(info);
        renameDebugStarted <= False;
    endrule

    // commit rule: fire when at least one commit can be done
    rule doCommit(rob.deqPort[0].deq_data.rob_inst_state == Executed);
        // stop superscalar commit after we
        // 1. need to start preparing cache/TLB (system inst or trap)
        // 2. redirect (by trap or system inst)
        // 3. update CSR
        // 4. an inst is not ready to commit
        Bool stop = False;

        // We apply actions (except ROB deq) at the end of the rule
        // Each loop iteration just figure out the action to do, but doesn't take action
        // This works because if an iteration can happe (stop == False),
        // then previous iterations cannot change any state (except ROB deq)
        // Apply actions at end makes rule splitting possible
        Maybe#(IType) prepareFlush = Invalid;
        Maybe#(CommitTrap) doTrap = Invalid;
        Maybe#(CommitRedirect) redirect = Invalid;
        CommitWrCsrf wrCsrf = Invalid;
        Maybe#(RenameErrInfo) renameError = Invalid;
        // track if we have committed an CSR inst or interrupt
        Bool commitCsrInstOrInterrupt = False;
        // incr committed inst cnt at the end of rule
        SupCnt comInstCnt = 0;
        SupCnt comUserInstCnt = 0;
`ifdef PERF_COUNT
        // incr some performance counter at the end of rule
        SupCnt brCnt = 0;
        SupCnt jmpCnt = 0;
        SupCnt jrCnt = 0;
`endif

        // compute what actions to take
        for(Integer i = 0; i < valueof(SupSize); i = i+1) begin
            if(!stop && rob.deqPort[i].canDeq) begin
                let x = rob.deqPort[i].deq_data;
                let inst_tag = rob.deqPort[i].getDeqInstTag;

                // check can be committed or not
                if(rob.deqPort[i].deq_data.rob_inst_state != Executed) begin
                    // inst not ready for commit, stop here
                    stop = True;
                end
                else begin
                    // inst can be committed, deq it
                    rob.deqPort[i].deq;

                    if (verbose) $display("[doCommit - %d] inst_tag = ", i, fshow(inst_tag), " ; ", fshow(x));

                    let iType = x.iType;
                    let trap = x.trap;
                    let pc = x.pc;
                    let csr = x.csr;
                    let claimed_phy_reg = x.claimed_phy_reg;
                    let fflags = x.fflags;
                    let spec_tag = x.spec_tag;
                    let will_dirty_fpu_state = x.will_dirty_fpu_state;

                    Addr nextPc = pc + 4; // if ppc in ROB is not updated in doFinishXXX rule, then must be pc+4 
                    Addr addr = ?; // page fault virtual addr
                    Data csrData = ?; // for Csr inst to write dst csr
                    case(x.ppc_vaddr_csrData) matches
                        tagged PPC .ppc: nextPc = ppc;
                        tagged VAddr .a: addr = a;
                        tagged CSRData .d: csrData = d;
                    endcase

                    if (isSystem(iType) || isValid(trap)) begin
                        // should start preparing cache/TLB, record iType
                        prepareFlush = Valid (iType);
                        stop = True; // stop commit
                    end

                    if (isValid(trap)) begin
                        // Don't do instruction, take trap instead, record the action
                        doTrap = Valid (CommitTrap {
                            pc: pc,
                            addr: addr,
                            trap: validValue(trap),
                            specTag: spec_tag,
                            instTag: inst_tag
                        });
                        stop = True; // stop commmit
                        
                        // record if we commit an interrupt
                        if(trap matches tagged Valid (tagged Interrupt .inter)) begin
                            commitCsrInstOrInterrupt = True;
                        end

                        // We should not commit renaming here
                        // There are two cases for an inst to have trap
                        // 1. It gets the trap at the front end, so it has not claimed any phy reg
                        // 2. It is a memory access that get fault, it must have already killed its own renaming
                        if(claimed_phy_reg) begin
                            case(iType)
                                Ld, St, Lr, Sc, Amo: noAction;
                                default: begin
                                    // send debug msg for rename error
                                    if(!isValid(renameError)) begin
                                        renameError = Valid (RenameErrInfo {
                                            err: TrapCommit,
                                            pc: pc,
                                            iType: iType,
                                            trap: trap,
                                            specTag: spec_tag,
                                            specBits: x.spec_bits
                                        });
                                        $fdisplay(stderr, "[doCommit - trap] RENAME ERROR DETECTED!");
                                    end
                                end
                            endcase
                        end
                    end
                    else begin
                        // Do instruction commit
                        // write csrf: don't write if redirect for Sret or Mret
                        if(iType != Sret && iType != Mret) begin
                            if(csr matches tagged Valid .idx) begin
                                wrCsrf = CsrInst (tuple2(idx, csrData));
                                stop = True; // stop commit
                            end
                            else if(csrf.fpuInstNeedWr(fflags, will_dirty_fpu_state)) begin
                                wrCsrf = FpuInst (fflags);
                                stop = True; // stop commit
                            end
                        end

                        // record if we commit a CSR inst
                        if(iType == Csr) begin
                            commitCsrInstOrInterrupt = True;
                        end

                        // every inst here should have been renamed, and won't kill itself
                        // so we always commit renaming
                        regRenamingTable.commit[i].commit;
                        // send debug msg for rename error
                        if(!claimed_phy_reg && !isValid(renameError)) begin
                            renameError = Valid (RenameErrInfo {
                                err: NonTrapCommit,
                                pc: pc,
                                iType: iType,
                                trap: trap,
                                specTag: spec_tag,
                                specBits: x.spec_bits
                            });
                            $fdisplay(stderr, "[doCommit - not trap] RENAME ERROR DETECTED!");
                        end

                        // Redirect (Sret and Mret redirect pc is got from CSRF)
                        if (iType == Sret || iType == Mret || doReplay(iType)) begin
                            // record info for redirect
                            redirect = Valid (CommitRedirect {
                                iType: iType,
                                nextPc: nextPc,
                                specTag: spec_tag,
                                instTag: inst_tag
                            });
                            stop = True; // stop commit
                        end

                        // inst commit counter
                        comInstCnt = comInstCnt + 1;
                        if(csrf.decodeInfo.prv == 0) begin
                            comUserInstCnt = comUserInstCnt + 1; // user space inst
                        end

`ifdef PERF_COUNT
                        // performance counter
                        case(iType)
                            Br: brCnt = brCnt + 1;
                            J : jmpCnt = jmpCnt + 1;
                            Jr: jrCnt = jrCnt + 1;
                        endcase
`endif
                    end
                end
            end
        end

        // flush TLB/Cache and wait SB flush and DTLB finish writes
        if(prepareFlush matches tagged Valid .iType) begin
            // Do Stuff in a later cycle because the vm update needs to see the effects of writes done in this rule
            if (iType == SFence) begin
                inIfc.setFlushTlbs;
            end
            inIfc.setUpdateVMInfo;
            // always wait store buffer to be empty
            when(stb.isEmpty, noAction);
            // We wait TLB to finish all requests and become sync with memory.
            // Notice that currently TLB is read only, so TLB is always in sync
            // with memory (i.e., there is no write to commit to memory). Since
            // all insts younger than this one have been killed, nothing can be
            // issued to D TLB at this time. Since fetch stage is set to wait
            // for redirect, fetch1 stage is stalled, and nothing can be issued
            // to I TLB at this time.  Therefore, we just need to make sure
            // that I and D TLBs are not handling any miss req. Besides, when I
            // and D TLBs do not have any miss req, L2 TLB must be idling.
            when(inIfc.tlbNoPendingReq, noAction);
            // yield load reservation in cache
            inIfc.setFlushReservation;
        end

        // record if we commit an interrupt or CSR inst
        if(commitCsrInstOrInterrupt) begin
            inIfc.commitCsrInstOrInterrupt;
        end

        (* split *)
        if(doTrap matches tagged Valid .t) (* nosplit *) begin
            // handle trap
            let trap_pc <- csrf.trap(t.trap, t.pc, t.addr);
            inIfc.redirect_action(trap_pc, t.specTag, t.instTag);
`ifdef PERF_COUNT
            // performance counter
            if(inIfc.doStats) begin
                trapCnt.incr(1);
            end
`endif
        end
        else (* nosplit *) begin
            // write CSRF: don't write if we redirect for sret or mret
            // (only one of wrte csrf, sret, mret can happen)
            Bool sret = False;
            Bool mret = False;
            if(redirect matches tagged Valid .r) begin
                sret = r.iType == Sret;
                mret = r.iType == Mret;
            end
            if(!sret && !mret) begin
                case(wrCsrf) matches
                    tagged CsrInst {.idx, .data}: begin
                        csrf.csrInstWr(idx, data);
                    end
                    tagged FpuInst .ff: begin
                        csrf.fpuInstWr(ff);
                    end
                endcase
            end
            // incr inst cnt
            csrf.incInstret(comInstCnt);
            // do redirect
            (* split *)
            if(redirect matches tagged Valid .r) (* nosplit *) begin
                if(r.iType == Sret) begin
                    let next_pc <- csrf.sret;
                    inIfc.redirect_action(next_pc, r.specTag, r.instTag);
`ifdef PERF_COUNT
                    // performance counter
                    if(inIfc.doStats) begin
                        sretCnt.incr(1);
                    end
`endif
                end
                else if(r.iType == Mret) begin
                    let next_pc <- csrf.mret;
                    inIfc.redirect_action(next_pc, r.specTag, r.instTag);
`ifdef PERF_COUNT
                    // performance counter
                    if(inIfc.doStats) begin
                        mrtsCnt.incr(1);
                    end
`endif
                end
                else begin
                    inIfc.redirect_action(r.nextPc, r.specTag, r.instTag);
                end
`ifdef PERF_COUNT
                // performance counter
                if(inIfc.doStats) begin
                    comRedirectCnt.incr(1);
                end
`endif
            end
        end

        // set rename error
        if(canSetRenameErr && isValid(renameError)) begin
            renameErrInfo <= renameError;
        end

`ifdef CHECK_DEADLOCK
        commitInst.send; // ROB head is removed
        if(comUserInstCnt > 0) begin
            commitUserInst.send;
        end
`endif

`ifdef PERF_COUNT
        // performance counter
        if(inIfc.doStats) begin
            // branch stats
            comBrCnt.incr(zeroExtend(brCnt));
            comJmpCnt.incr(zeroExtend(jmpCnt));
            comJrCnt.incr(zeroExtend(jrCnt));
            // inst count stats
            instCnt.incr(zeroExtend(comInstCnt));
            userInstCnt.incr(zeroExtend(comUserInstCnt));
            if(comUserInstCnt > 1) begin
                supComUserCnt.incr(1);
            end
        end
`endif

`ifdef VERIFICATION_PACKETS
        // FIXME: currently no verify packet
        /*
        let inst = x.inst;
        let data = x.result_data;
        let dst = x.arch_reg_dst;
        let packet = VerificationPacket{
            skipped_packets: skippedVerificationPackets,
            pc: pc,
            addr: pc+1, // FIXME: WHAT DO WE PUT HERE? was nextPc
            data1: data,
            data2: data,
            instruction: inst,
            // dst: ({pack(isValid(dst)),pack(fromMaybe(?,dst).indx)}),
            dst: pack(dst),
            trap: False,
            trap_type: 0
        };

        if (isValid(trap)) begin
            packet.trap = True;
            packet.trap_type = (case (fromMaybe(?,trap)) matches
                tagged Exception .e: (zeroExtend(pack(e)));
                tagged Interrupt .i: (zeroExtend(pack(i)) | 8'h80);
            endcase);
        end

        if (iType == Ld || iType == St || iType == Lr || iType == Sc || iType == Amo) begin
            packet.addr = addr;
        end

        if (iType == Csr) begin
            packet.addr = zeroExtend(pack(fromMaybe(?, csr))); // FIXME: csr_dst
            packet.data2 = csrData;
        end

        // figure out if this is a synchronization packet
        Bool synchronization_packet = False;
        if (trap matches tagged Valid (tagged Interrupt .i) &&& (i == HostInterrupt || i == TimerInterrupt)) begin
            synchronization_packet = True;
        end else if (isValid(csr)) begin
            case (fromMaybe(?,csr))
                CSRmtime, CSRstime, CSRstimew, CSRtime, CSRtimew, CSRcycle, CSRcyclew, CSRinstret, CSRinstretw, CSRmip, CSRmfromhost:
                    synchronization_packet = True;
            endcase
        end

        if ((synchronization_packet && sendSynchronizationPackets) || (verificationPacketsToIgnore == 0)) begin
            verifyFifo.enq(packet);
            skippedVerificationPackets <= 0;
        end else begin
            skippedVerificationPackets <= skippedVerificationPackets + 1;
        end
        if ((verificationPacketsToIgnore != 0) && (verificationPacketsToIgnore != '1)) begin
            verificationPacketsToIgnore <= verificationPacketsToIgnore - 1;
        end
        */
`endif
    endrule


    method Action initVerify(Bool sendSyncPack, Bit#(64) skippedPack, Bit#(64) packToIgore);
`ifdef VERIFICATION_PACKETS
        sendSynchronizationPackets <= sendSyncPack;
        skippedVerificationPackets <= skippedPack;
        verificationPacketsToIngore <= packToIgore;
`else
        noAction;
`endif
    endmethod

`ifdef VERIFICATION_PACKETS
    method ActionValue#(VerificationPacket) debug_verify;
        if (verbose) $display("Sending to host: ", verifyFifo.first.pc);
        verifyFifo.deq;
        return verifyFifo.first;
    endmethod
`else
    method ActionValue#(VerificationPacket) debug_verify if(False);
        return ?;
    endmethod
`endif

    method Data getPerf(ComStagePerfType t);
        return (case(t)
`ifdef PERF_COUNT
            InstCnt: instCnt;
            UserInstCnt: userInstCnt;
            SupComUserCnt: supComUserCnt;
            ComBrCnt: comBrCnt;
            ComJmpCnt: comJmpCnt;
            ComJrCnt: comJrCnt;
            ComRedirect: comRedirectCnt;
            TrapCnt: trapCnt;
            SretCnt: sretCnt;
            MrtsCnt: mrtsCnt;
`endif
            default: 0;
        endcase);
    endmethod

`ifdef CHECK_DEADLOCK
    interface commitInstStuck = toGet(commitInstStuckQ);
    interface commitUserInstStuck = toGet(commitUserInstStuckQ);
`else
    interface commitInstStuck = nullGet;
    interface commitUserInstStuck = nullGet;
`endif

    method Action startRenameDebug if(!renameDebugStarted);
        renameDebugStarted <= True;
    endmethod
    interface renameErr = toGet(renameErrQ);
endmodule
