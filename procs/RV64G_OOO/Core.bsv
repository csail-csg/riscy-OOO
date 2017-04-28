
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
import BuildVector::*;
import DefaultValue::*;
import ClientServer::*;
import GetPut::*;
import Assert::*;
import Cntrs::*;
import ConfigReg::*;
import FIFO::*;
import Fifo::*;
import Ehr::*;
import Connectable::*;
import ConnectalConfig::*;

import Types::*;
import ProcTypes::*;
import CacheUtils::*;
import TlbTypes::*;
import SynthParam::*;
import VerificationPacket::*;
import Performance::*;
import HasSpecBits::*;
import Exec::*;
import FetchStage::*;
import ITlb::*;
import DTlb::*;
import L2Tlb::*;
import TlbConnect::*;
import EpochManager::*;
import CsrFile::*;
import PhysRFile::*;
import RFileSynth::*;
import RenamingTable::*;
import ReorderBuffer::*;
import ReorderBufferSynth::*;
import Scoreboard::*;
import ScoreboardSynth::*;
import SpecTagManager::*;
import Fpu::*;
import MulDiv::*;
import ReservationStationEhr::*;
import ReservationStationAlu::*;
import ReservationStationMem::*;
import ReservationStationFpuMulDiv::*;
import AluExePipeline::*;
import FpuMulDivExePipeline::*;
import MemExePipeline::*;
import SpecLSQ::*;
import StoreBuffer::*;
import GlobalSpecUpdate::*;
import CCTypes::*;
import L1CoCache::*;
import L1Bank::*;
import IBank::*;
import RenameStage::*;
import CommitStage::*;
import Bypass::*;

interface CoreReq;
    method Action start(Bit#(64) pc, Bool ipi_wait_msip_zero, Bit#(64) pack_ignore, Bool sync_pack);
    method Action from_host(Bit#(64) v);
    method Action perfReq(PerfLocation loc, PerfType t);
endinterface

interface CoreIndInv;
    method ActionValue#(Bit#(64)) to_host;
    method ActionValue#(VerificationPacket) debug_verify;
    method ActionValue#(ProcPerfResp) perfResp;
    method ActionValue#(void) terminate;
endinterface

interface CoreIPI;
    method Action recvIPI;
    method ActionValue#(CoreId) sendIPI;
endinterface

interface CoreDeadlock;
    interface Get#(L1CRqStuck) dCacheCRqStuck;
    interface Get#(L1PRqStuck) dCachePRqStuck;
    interface Get#(ICRqStuck) iCacheCRqStuck;
    interface Get#(IPRqStuck) iCachePRqStuck;
    interface Get#(RenameStuck) renameInstStuck;
    interface Get#(RenameStuck) renameCorrectPathStuck;
    interface Get#(CommitStuck) commitInstStuck;
    interface Get#(CommitStuck) commitUserInstStuck;
    interface Get#(void) checkStarted;
    method Action setCheckStartInstNum(Data n);
endinterface

interface CoreRenameDebug;
    interface Get#(RenameErrInfo) renameErr;
endinterface

interface Core;
    // core request & indication
    interface CoreReq coreReq;
    interface CoreIndInv coreIndInv;
    // inter-proc interrupt
    interface CoreIPI ipi;
    // coherent caches to LLC
    interface ChildCacheToParent#(L1Way, void) dCacheToParent;
    interface ChildCacheToParent#(L1Way, void) iCacheToParent;
    // DMA to LLC
    interface TlbMemClient tlbToMem;
    // detect deadlock: only in use when macro CHECK_DEADLOCK is defined
    interface CoreDeadlock deadlock;
    // debug rename
    interface CoreRenameDebug renameDebug;
endinterface

// flush pipeline
typedef enum {
    Invalid,
    HostToCsrf,
    InterProcInterrupt
} FlushPipeSrc deriving(Bits, Eq, FShow);

typedef enum {FlushROB, FlushSTB, FlushCache} FlushPipeFSM deriving(Bits, Eq, FShow);

// fixpoint to instantiate modules
interface CoreFixPoint;
    interface Vector#(AluExeNum, AluExePipeline) aluExeIfc;
    interface FpuMulDivExePipeline fpuMulDivExeIfc;
    interface MemExePipeline memExeIfc;
    method Action redirect_action(Addr trap_pc, Maybe#(SpecTag) spec_tag, InstTag inst_tag);
    interface Reg#(Bool) doStatsIfc;
endinterface

(* synthesize *)
module mkCore#(CoreId coreId)(Core);
    let verbose = True;
    Reg#(Bool) outOfReset <- mkReg(False);
    rule rl_outOfReset if (!outOfReset);
        $fwrite(stderr, "mkProc came out of reset\n");
        outOfReset <= True;
    endrule

    Reg#(Bool) started <- mkReg(False);

    // front end
    FetchStage fetchStage <- mkFetchStage;
    ITlb iTlb = fetchStage.iTlbIfc;
    ICoCache iMem = fetchStage.iMemIfc;

    // back end
    RFileSynth rf <- mkRFileSynth;
    CsrFile csrf <- mkCsrFileWithId(zeroExtend(coreId)); // hartid in CSRF should be core id
    RegRenamingTable regRenamingTable <- mkRegRenamingTable;
    EpochManager epochManager <- mkEpochManager;
    SpecTagManager specTagManager <- mkSpecTagManager;
    ReorderBufferSynth rob <- mkReorderBufferSynth; // super v.s. single scalar ROB is controlled by macro SUP_ROB

    // We have two types of pipeline:
    // - One type recv bypass and early wakeup: we call it **aggressive** pipeline
    // - Other type does not check bypass or early wakeup: we call it **conservative** pipeline
    // - all pipelines are aggressive now
    // We have two scoreboards: one conservative and other aggressive
    // - Aggressive pipeline checks aggressive sb at rename stage,
    //   and checks conservative sb at reg read stage
    // - Conservative pipeline checks conservative sb at rename stage
    // - Both sb must be set ready by all pipelines
    // - Conservative sb can only be set when data is written into rf
    // - Aggressive sb can be set early by a pipeline which sends out bypass
    ScoreboardCons sbCons <- mkScoreboardCons; // conservative sb
    ScoreboardAggr sbAggr <- mkScoreboardAggr; // aggressive sb

    // fix point module to instantiate other function units
    module mkCoreFixPoint#(CoreFixPoint fix)(CoreFixPoint);
        // spec update
        Vector#(AluExeNum, SpeculationUpdate) aluSpecUpdate;
        for(Integer i = 0; i < valueof(AluExeNum); i = i+1) begin
            aluSpecUpdate[i] = fix.aluExeIfc[i].specUpdate;
        end
        GlobalSpecUpdate#(CorrectSpecPortNum, ConflictWrongSpecPortNum) globalSpecUpdate <- mkGlobalSpecUpdate(
            joinSpeculationUpdate(append(
                vec(
                    regRenamingTable.specUpdate,
                    specTagManager.specUpdate,
                    fix.fpuMulDivExeIfc.specUpdate,
                    fix.memExeIfc.specUpdate
                ),
                aluSpecUpdate
            )),
            rob.specUpdate
        );

        // whether perf data is collected
        Reg#(Bool) doStatsReg <- mkConfigReg(False); 

        // redirect func
        function Action redirectFunc(Addr trap_pc, Maybe#(SpecTag) spec_tag, InstTag inst_tag );
        action
            if (verbose) $fdisplay(stdout, "[redirect_action] new pc = 0x%8x, spec_tag = ", trap_pc, fshow(spec_tag));
            epochManager.redirect;
            fetchStage.redirect(trap_pc);
            if (spec_tag matches tagged Valid .valid_spec_tag) begin
                globalSpecUpdate.incorrectSpec(valid_spec_tag, inst_tag);
            end
        endaction
        endfunction

        // write aggressive elements
        function Action writeAggr(Integer wrAggrPort, PhyRIndx dst);
        action
            sbAggr.setReady[wrAggrPort].put(dst);
            for(Integer i = 0; i < valueof(AluExeNum); i = i+1) begin
                fix.aluExeIfc[i].rsAluIfc.setRegReady[wrAggrPort].put(Valid (dst));
            end
            fix.fpuMulDivExeIfc.rsFpuMulDivIfc.setRegReady[wrAggrPort].put(Valid (dst));
            fix.memExeIfc.rsMemIfc.setRegReady[wrAggrPort].put(Valid (dst));
        endaction
        endfunction

        // write conservative elements
        function Action writeCons(Integer wrConsPort, PhyRIndx dst, Data data);
        action
            rf.write[wrConsPort].wr(dst, data);
            sbCons.setReady[wrConsPort].put(dst);
        endaction
        endfunction

        Vector#(AluExeNum, AluExePipeline) aluExe;
        for(Integer i = 0; i < valueof(AluExeNum); i = i+1) begin
            Vector#(2, SendBypass) sendBypassIfc; // exe and finish
            for(Integer sendPort = 0; sendPort < 2; sendPort = sendPort + 1) begin
                sendBypassIfc[sendPort] = (interface SendBypass;
                    method Action send(PhyRIndx dst, Data data);
                        // broadcast bypass
                        Integer recvPort = valueof(AluExeNum) * sendPort + i;
                        fix.fpuMulDivExeIfc.recvBypass[recvPort].recv(dst, data);
                        fix.memExeIfc.recvBypass[recvPort].recv(dst, data);
                        for(Integer j = 0; j < valueof(AluExeNum); j = j+1) begin
                            fix.aluExeIfc[j].recvBypass[recvPort].recv(dst, data);
                        end
                    endmethod
                endinterface);
            end
            let aluExeInput = (interface AluExeInput;
                method sbCons_lazyLookup = sbCons.lazyLookup[aluRdPort(i)].get;
                method rf_rd1 = rf.read[aluRdPort(i)].rd1;
                method rf_rd2 = rf.read[aluRdPort(i)].rd2;
                method csrf_rd = csrf.rd;
                method rob_getPC = rob.getOrigPC[i].get;
                method rob_getPredPC = rob.getOrigPredPC[i].get;
                method rob_setExecuted = rob.setExecuted_doFinishAlu[i].set;
                method fetch_train_predictors = fetchStage.train_predictors;
                method setRegReadyAggr = writeAggr(aluWrAggrPort(i));
                interface sendBypass = sendBypassIfc;
                method writeRegFile = writeCons(aluWrConsPort(i));
                method redirect_action = redirectFunc;
                method correctSpec = globalSpecUpdate.correctSpec[finishAluCorrectSpecPort(i)].put;
                method doStats = doStatsReg._read;
            endinterface);
            aluExe[i] <- mkAluExePipeline(aluExeInput);
        end

        let fpuMulDivExeInput = (interface FpuMulDivExeInput;
            method sbCons_lazyLookup = sbCons.lazyLookup[fpuMulDivRdPort].get;
            method rf_rd1 = rf.read[fpuMulDivRdPort].rd1;
            method rf_rd2 = rf.read[fpuMulDivRdPort].rd2;
            method csrf_rd = csrf.rd;
            method rob_setExecuted = rob.setExecuted_doFinishFpuMulDiv;
            method Action writeRegFile(PhyRIndx dst, Data data);
                writeAggr(fpuMulDivWrAggrPort, dst);
                writeCons(fpuMulDivWrConsPort, dst, data);
            endmethod
            method conflictWrongSpec = globalSpecUpdate.conflictWrongSpec[finishFpuMulDivConflictWrongSpecPort].put(?);
        endinterface);
        let fpuMulDivExe <- mkFpuMulDivExePipeline(fpuMulDivExeInput);

        let memExeInput = (interface MemExeInput;
            method sbCons_lazyLookup = sbCons.lazyLookup[memRdPort].get;
            method rf_rd1 = rf.read[memRdPort].rd1;
            method rf_rd2 = rf.read[memRdPort].rd2;
            method csrf_rd = csrf.rd;
            method rob_setExecuted_doFinishMem = rob.setExecuted_doFinishMem;
            method rob_setExecuted_deqLSQ = rob.setExecuted_deqLSQ;
            method rob_setLdSpecBit = rob.setLdSpecBit;
            method Action incrementEpochWithoutRedirect;
                epochManager.incrementEpochWithoutRedirect;
                // stop fetch until redirect
                fetchStage.setWaitRedirect;
            endmethod
            method setRegReadyAggr_cache = writeAggr(cacheWrAggrPort);
            method setRegReadyAggr_forward = writeAggr(forwardWrAggrPort);
            method writeRegFile_Ld = writeCons(ldWrConsPort);
            method writeRegFile_LrScAmo = writeCons(lrScAmoWrConsPort);
            method redirect_action = redirectFunc;
            method correctSpec_doFinishMem = globalSpecUpdate.correctSpec[finishMemCorrectSpecPort].put;
            method correctSpec_deqLSQ = globalSpecUpdate.correctSpec[deqLSQCorrectSpecPort].put;
            method incorrectSpec = globalSpecUpdate.incorrectSpec;
            method doStats = doStatsReg._read;
        endinterface);
        let memExe <- mkMemExePipeline(memExeInput);

        interface aluExeIfc = aluExe;
        interface fpuMulDivExeIfc = fpuMulDivExe;
        interface memExeIfc = memExe;
        method redirect_action = redirectFunc;
        interface doStatsIfc = doStatsReg;
    endmodule
    CoreFixPoint coreFix <- moduleFix(mkCoreFixPoint);

    Vector#(AluExeNum, ReservationStationAlu) reservationStationAlu;
    for(Integer i = 0; i < valueof(AluExeNum); i = i+1) begin
        reservationStationAlu[i] = coreFix.aluExeIfc[i].rsAluIfc;
    end
    ReservationStationFpuMulDiv reservationStationFpuMulDiv = coreFix.fpuMulDivExeIfc.rsFpuMulDivIfc;
    ReservationStationMem reservationStationMem = coreFix.memExeIfc.rsMemIfc;
    DTlb dTlb = coreFix.memExeIfc.dTlbIfc;
    SpecLSQ lsq = coreFix.memExeIfc.lsqIfc;
    StoreBuffer stb = coreFix.memExeIfc.stbIfc;
    DCoCache dMem = coreFix.memExeIfc.dMemIfc;

    // L2 TLB
    L2Tlb l2Tlb <- mkL2Tlb;
    mkTlbConnect(iTlb.toParent, dTlb.toParent, l2Tlb.toChildren);

    // flags to flush
    Reg#(Bool)  flush_caches <- mkReg(False);
    Reg#(Bool)  flush_tlbs <- mkReg(False);
    Reg#(Bool)  update_vm_info <- mkReg(False);
    Reg#(Bool)  flush_reservation <- mkReg(False);

    // HTIF
    Reg#(Bool) htifStall <- mkReg(False);
    FIFO#(Data) toHostQ <- mkFIFO1;
`ifdef FLUSH_CACHES_ON_HTIF
    Fifo#(1, Bit#(64)) csrftohost_flushfifo <- mkCFFifo;
`endif
    Fifo#(2, Data) fromHostQ <- mkCFFifo;

    // IPI
    FIFO#(CoreId) ipiOutQ <- mkFIFO1;
`ifdef FLUSH_CACHES_ON_IPI
    // we flush local caches when sending out IPI
    // (store buffer is flushed due to CSR inst which writes IPI CSR to send IPI)
    FIFO#(CoreId) ipiFlushQ <- mkFIFO1;
`endif
    Fifo#(2, void) ipiInQ <- mkCFFifo; // buffer income IPI
    Reg#(Bool) waitMsipZero <- mkConfigReg(False); // whether we wait msip == 0 for income IPI
    // use config reg to prevent schedule cycle

    // flush pipeline when there is incoming fromhost or inter-proc interrupt msg
    // this avoids races between incoming msg and pending inst
    Reg#(FlushPipeSrc) flushPipeSrc <- mkReg(Invalid);
    Reg#(FlushPipeFSM) flushPipeState <- mkRegU;
    // wires are used to avoid scheduling cycles
    Wire#(Bool) robFlushed <- mkBypassWire;
    Wire#(Bool) stbFlushed <- mkBypassWire;

    // performance counters
    Reg#(Bool) doStats = coreFix.doStatsIfc; // whether data is collected
`ifdef PERF_COUNT
    // OOO execute stage
    // some in AluExePipeline and MemExePipeline
    // htif stalls
    Count#(Data) htifStallCnt <- mkCount(0);
    Count#(Data) htifStallLat <- mkCount(0);

    // commit stage (many in CommitStage.bsv)
    // cycle
    Count#(Data) cycleCnt <- mkCount(0);

    // FIFOs to connect performance counters
    FIFO#(ExeStagePerfType) exePerfReqQ <- mkFIFO1;
    FIFO#(ComStagePerfType) comPerfReqQ <- mkFIFO1;
    Fifo#(1, PerfResp#(ExeStagePerfType)) exePerfRespQ <- mkCFFifo;
    Fifo#(1, PerfResp#(ComStagePerfType)) comPerfRespQ <- mkCFFifo;

    // FIFO of perf resp
    FIFO#(ProcPerfResp) perfRespQ <- mkFIFO1;
`endif
    // FIFO of perf req
    FIFO#(ProcPerfReq) perfReqQ <- mkFIFO1;

    // -- End of performance counters

`ifdef CHECK_DEADLOCK
    // when to start deadlock checking
    Reg#(Bool) startDeadlockCheck <- mkReg(False);
    Reg#(Maybe#(Data)) deadlockCheckStartInstNum <- mkReg(Invalid); // check start after committing these number of inst
    FIFO#(void) deadlockCheckStartedQ <- mkFIFO;

    rule doStartDeadlockCheck(
        deadlockCheckStartInstNum matches tagged Valid .n &&&
        n < csrf.rd(CSRinstret) &&& !startDeadlockCheck &&& started
    );
        startDeadlockCheck <= True;
        deadlockCheckStartedQ.enq(?);
    endrule
`endif

    // Rename stage
    let renameInput = (interface RenameInput;
        interface fetchIfc = fetchStage;
        interface robIfc = rob;
        interface rtIfc = regRenamingTable;
        interface sbConsIfc = sbCons;
        interface sbAggrIfc = sbAggr;
        interface csrfIfc = csrf;
        interface emIfc = epochManager;
        interface smIfc = specTagManager;
        interface rsAluIfc = reservationStationAlu;
        interface rsFpuMulDivIfc = reservationStationFpuMulDiv;
        interface rsMemIfc = reservationStationMem;
        interface lsqIfc = lsq;
        method isHtifStall = htifStall._read;
        method notFlushingPipe = flushPipeSrc == Invalid;
        method Bool checkDeadlock;
`ifdef CHECK_DEADLOCK
            return startDeadlockCheck;
`else
            return False;
`endif
        endmethod
        method doStats = coreFix.doStatsIfc._read;
    endinterface);
    RenameStage renameStage <- mkRenameStage(renameInput);

    // commit stage
    let commitInput = (interface CommitInput;
        interface robIfc = rob;
        interface rtIfc = regRenamingTable;
        interface csrfIfc = csrf;
        interface stbIfc = stb;
        method tlbNoPendingReqOrWrite = iTlb.noPendingReq && dTlb.noPendingReqOrWrite && l2Tlb.noPendingReqOrWrite;
        method isHtifStall = htifStall._read;
        method setFlushCaches = flush_caches._write(True);
        method setFlushTlbs = flush_tlbs._write(True);
        method setUpdateVMInfo = update_vm_info._write(True);
        method setFlushReservation = flush_reservation._write(True);
        method redirect_action = coreFix.redirect_action;
        method doStats = coreFix.doStatsIfc._read;
        method Bool checkDeadlock;
`ifdef CHECK_DEADLOCK
            return startDeadlockCheck;
`else
            return False;
`endif
        endmethod
    endinterface);
    CommitStage commitStage <- mkCommitStage(commitInput);

    // send rob enq time to reservation stations
    (* fire_when_enabled, no_implicit_conditions *)
    rule sendRobEnqTime;
        InstTime t = rob.getEnqTime;
        reservationStationMem.setRobEnqTime(t);
        reservationStationFpuMulDiv.setRobEnqTime(t);
        for(Integer i = 0; i < valueof(AluExeNum); i = i+1) begin
            reservationStationAlu[i].setRobEnqTime(t);
        end
    endrule

    // preempt has 2 functions here
    // 1. break scheduling cycles
    // 2. XXX since csrf is configReg now, we should not let this rule fire together with doCommit
    // because we read csrf here and write csrf in doCommit
    (* preempts = "prepareCachesAndTlbs, commitStage.doCommit" *)
    rule prepareCachesAndTlbs(flush_caches || flush_reservation || flush_tlbs || update_vm_info);
        if (flush_caches) begin
            flush_caches <= False;
            iMem.flush;
            dMem.flush;
        end
        if (flush_reservation) begin
            flush_reservation <= False;
            dMem.resetLinkAddr;
        end
        if (flush_tlbs) begin
            flush_tlbs <= False;
            iTlb.flush;
            dTlb.flush;
        end
        if (update_vm_info) begin
            update_vm_info <= False;
            let vmI = csrf.vmI;
            let vmD = csrf.vmD;
            iTlb.updateVMInfo(vmI);
            dTlb.updateVMInfo(vmD);
            l2Tlb.updateVMInfo(vmI, vmD);
        end
    endrule

    rule readyToFetch(
        !flush_caches && !flush_reservation && !flush_tlbs && !update_vm_info
        && iMem.flush_done && dMem.flush_done && iTlb.flush_done && dTlb.flush_done
    );
        fetchStage.done_flushing();
    endrule

    // [sizhuo] this debug rule blocks redirection at doCommit
    //rule debugBackground(verbose);
    //if (htifStall) begin
    //    $fdisplay(stdout, "stall htif");
    //end
    //if (from_fetch_fifo.notEmpty) begin
    //    $fdisplay(stdout, fshow(from_fetch_fifo.first));
    //end
    //if (rob.isEmpty) begin
    //    $fdisplay(stdout, "The ROB is empty");
    //end
    //endrule


    // send to host msg
    rule csrfToHost;
        let ret <- csrf.csrfToHost;
        if(verbose) $display("[csrfToHost] core %d, val %x", coreId, ret);
        // Don't stall for reading from the terminal
        // Thomas says this is awful
        // [sizhuo] do not compare lower bits, they may be garbage
        if (truncateLSB(ret) != 16'h0100) begin
            htifStall <= True;
`ifdef FLUSH_CACHES_ON_HTIF
            // flush caches
            // iMem.flush;
            // dMem.flush;
            flush_caches <= True;
`endif

`ifdef PERF_COUNT
            // performance counter
            if(doStats) begin
                htifStallCnt.incr(1);
            end
`endif
        end

`ifdef FLUSH_CACHES_ON_HTIF
        csrftohost_flushfifo.enq(ret);
`else
        toHostQ.enq(ret);
`endif
    endrule

`ifdef FLUSH_CACHES_ON_HTIF
    rule csrfToHost_wait_for_flush((!flush_caches) && iMem.flush_done && dMem.flush_done);
        let ret = csrftohost_flushfifo.first;
        csrftohost_flushfifo.deq;
        toHostQ.enq(ret);
        if(verbose) $display("[csrfToHost_wait_for_flush] core %d, val %x", coreId, ret);
    endrule

    //(* preempts = "hostToCsrf_wait_for_flush, doCommit" *)
    //rule hostToCsrf_wait_for_flush((!flush_caches) && iMem.flush_done && dMem.flush_done);
    //    let v = hosttocsrf_flushfifo.first;
    //    hosttocsrf_flushfifo.deq;
    //    csrf.hostToCsrf(v);
    //    htifStall <= False;
    //endrule
`endif

    // try to process fromhost msg, only do this when mfromhost CSR is 0
    // and we are not processing other msg (e.g. other IPI/fromhost)
    rule doFromHost(flushPipeSrc == Invalid && fromHostQ.notEmpty && csrf.fromHostZero);
        flushPipeState <= FlushROB;
        flushPipeSrc <= HostToCsrf;
        // XXX don't deq fromHostQ
        // since mfromhost CSR may be non-zero after flushing pipeline
    endrule

    // send IPI to outside
    rule doSendIPI;
        let c <- csrf.sendIPI;
        if(verbose) $display("[doSendIPI] core %d, val %x", coreId, c);
`ifdef FLUSH_CACHES_ON_IPI
        ipiFlushQ.enq(c);
        flush_caches <= True;
`else
        ipiOutQ.enq(c);
`endif
    endrule

`ifdef FLUSH_CACHES_ON_IPI
    rule doSendIPI_waitFlush(!flush_caches && iMem.flush_done && dMem.flush_done);
        let c <- toGet(ipiFlushQ).get;
        ipiOutQ.enq(c);
        if(verbose) $display("[doSendIPI_waitFlush] core %d, val %x", coreId, c);
    endrule
`endif

    // try to process income IPI by first flushing the pipeline
    // only do this when no other msg is flushing pipeline
    // it is configurable whether we also wait misp to be zero
    Bool canAcceptIPI = !waitMsipZero || csrf.msipZero;
    rule doRecvIPI(flushPipeSrc == Invalid && ipiInQ.notEmpty && canAcceptIPI);
        flushPipeState <= FlushROB;
        flushPipeSrc <= InterProcInterrupt;
        // XXX don't deq ipiInQ
        // since msip CSR may be non-zero after flushing pipeline
    endrule

    // set ROB/STB flushed signals 
    (* fire_when_enabled, no_implicit_conditions *)
    rule setRobFlushed;
        robFlushed <= rob.isEmpty_ehrPort0;
    endrule
    (* fire_when_enabled, no_implicit_conditions *)
    rule setStbFlushed;
        stbFlushed <= stb.isEmpty;
    endrule

    // FSM for flushing pipeline for incoming fromhost or ipi msg
    rule doFlushPipe_ROB(flushPipeSrc != Invalid && flushPipeState == FlushROB && robFlushed);
        flushPipeState <= FlushSTB;
    endrule

    rule doFlushPipe_STB(flushPipeSrc != Invalid && flushPipeState == FlushSTB && stbFlushed);
        flushPipeState <= FlushCache;
        flush_caches <= True;
    endrule

    rule doFlushPipe_Cache_HostToCsrf(
        flushPipeSrc == HostToCsrf && flushPipeState == FlushCache
        && !flush_caches && iMem.flush_done && dMem.flush_done
    );
        flushPipeSrc <= Invalid; // resume pipeline
        if(csrf.fromHostZero) begin
            // mfromhost is 0, so write it and the fromhost msg is done
            fromHostQ.deq;
            csrf.hostToCsrf(fromHostQ.first);
            htifStall <= False;
        end
        // else: don't touch fromHostQ, we will automatically retry when mfromhost == 0
    endrule

    rule doFlushPipe_Cache_InterProcInterrupt(
        flushPipeSrc == InterProcInterrupt && flushPipeState == FlushCache
        && !flush_caches && iMem.flush_done && dMem.flush_done
    );
        flushPipeSrc <= Invalid; // resume pipeline
        if(canAcceptIPI) begin
            // msip is 0 or we don't wait for msip, so write it and IPI msg is done
            ipiInQ.deq;
            csrf.recvIPI;
        end
        // else: don't touch ipiInQ, we will automatically retry when msip == 0
    endrule

`ifdef PERF_COUNT
    // incr cycle count
    rule incCycleCnt(doStats);
        cycleCnt.incr(1);
    endrule

    // incr htif stall time
    rule incHtifStall(started && htifStall && doStats);
        htifStallLat.incr(1);
    endrule

    // broadcast whether we should collect data
    rule broadcastDoStats;
        let stats = csrf.doPerfStats;
        doStats <= stats;
        iMem.perf.setStatus(stats);
        dMem.perf.setStatus(stats);
        iTlb.perf.setStatus(stats);
        dTlb.perf.setStatus(stats);
        fetchStage.perf.setStatus(stats);
    endrule

    // dispatch perf req
    rule dispathPerfReq_ICache;
        perfReqQ.deq;
        let r = perfReqQ.first;
        case(r.loc)
            ICache: begin
                iMem.perf.req(unpack(truncate(r.pType)));
            end
            DCache: begin
                dMem.perf.req(unpack(truncate(r.pType)));
            end
            ITlb: begin
                iTlb.perf.req(unpack(truncate(r.pType)));
            end
            DTlb: begin
                dTlb.perf.req(unpack(truncate(r.pType)));
            end
            DecStage: begin
                fetchStage.perf.req(unpack(truncate(r.pType)));
            end
            ExeStage: begin
                exePerfReqQ.enq(unpack(truncate(r.pType)));
            end
            ComStage: begin
                comPerfReqQ.enq(unpack(truncate(r.pType)));
            end
            default: begin
                $fwrite(stderr, "[WARNING] unrecognzied perf req location ", fshow(r.loc), "\n");
            end
        endcase
    endrule

    // handle perf req: exe stage
    rule readPerfCnt_Exe;
        function Data getAluCnt(ExeStagePerfType pType);
            Data cnt = 0;
            for(Integer i = 0; i < valueof(AluExeNum); i = i+1) begin
                cnt = cnt + coreFix.aluExeIfc[i].getPerf(pType);
            end
            return cnt;
        endfunction
        let pType <- toGet(exePerfReqQ).get;
        Data data = (case(pType)
            SupRenameCnt: renameStage.getPerf(pType);
            ExeRedirectBr, ExeRedirectJr, ExeRedirectOther: getAluCnt(pType);
            ExeKillLd, ExeTlbExcep: coreFix.memExeIfc.getPerf(pType);
            HtifStallCnt: htifStallCnt;
            HtifStallLat: htifStallLat;
            default: 0;
        endcase);
        exePerfRespQ.enq(PerfResp {
            pType: pType,
            data: data
        });
    endrule

    // handle perf req: com stage
    rule readPerfCnt_Com;
        let pType <- toGet(comPerfReqQ).get;
        Data data = (case(pType)
            CycleCnt: cycleCnt;
            default: commitStage.getPerf(pType);
        endcase);
        comPerfRespQ.enq(PerfResp {
            pType: pType,
            data: data
        });
    endrule

    // gather perf resp
    rule gatherPerfResp;
        Maybe#(ProcPerfResp) resp = Invalid;
        if(iMem.perf.respValid) begin
            let r <- iMem.perf.resp;
            resp = Valid(ProcPerfResp {
                loc: ICache,
                pType: zeroExtend(pack(r.pType)),
                data: r.data
            });
        end
        else if(dMem.perf.respValid) begin
            let r <- dMem.perf.resp;
            resp = Valid(ProcPerfResp {
                loc: DCache,
                pType: zeroExtend(pack(r.pType)),
                data: r.data
            });
        end
        if(iTlb.perf.respValid) begin
            let r <- iTlb.perf.resp;
            resp = Valid(ProcPerfResp {
                loc: ITlb,
                pType: zeroExtend(pack(r.pType)),
                data: r.data
            });
        end
        if(dTlb.perf.respValid) begin
            let r <- dTlb.perf.resp;
            resp = Valid(ProcPerfResp {
                loc: DTlb,
                pType: zeroExtend(pack(r.pType)),
                data: r.data
            });
        end
        else if(fetchStage.perf.respValid) begin
            let r <- fetchStage.perf.resp;
            resp = Valid(ProcPerfResp {
                loc: DecStage,
                pType: zeroExtend(pack(r.pType)),
                data: r.data
            });
        end
        else if(exePerfRespQ.notEmpty) begin
            let r <- toGet(exePerfRespQ).get;
            resp = Valid(ProcPerfResp {
                loc: ExeStage,
                pType: zeroExtend(pack(r.pType)),
                data: r.data
            });
        end
        else if(comPerfRespQ.notEmpty) begin
            let r <- toGet(comPerfRespQ).get;
            resp = Valid(ProcPerfResp {
                loc: ComStage,
                pType: zeroExtend(pack(r.pType)),
                data: r.data
            });
        end
        // enq to resp Q
        if(resp matches tagged Valid .r) begin
            perfRespQ.enq(r);
        end
    endrule
`endif

    interface CoreReq coreReq;
        method Action start(
            Bit#(64) startpc, Bool ipi_wait_msip_zero,
            Bit#(64) verification_packets_to_ignore,
            Bool send_synchronization_packets
        );
            commitStage.initVerify(send_synchronization_packets, 0, verification_packets_to_ignore);
            fetchStage.start(startpc);
            started <= True;
            waitMsipZero <= ipi_wait_msip_zero;
            csrf.hostToCsrf(0);
            // start rename debug
            commitStage.startRenameDebug;
        endmethod

        //method Action noFlying() if (axi_mem_bridge.numberFlyingOperations == 0);
        //    noAction;
        //endmethod

        //method Action stop();
        //    axi_mem_bridge.flushRespReqMem;
        //    fetchStage.stop();
        //    started <= False;
        //endmethod

        method Action from_host(Bit#(64) v);
            fromHostQ.enq(v);
        endmethod

    //`ifdef CONNECTAL_MEMORY
    //    method Action initSharedMem(Bit#(32) refPointer, Addr memSize);
    //        axi_mem_bridge.initSharedMem(refPointer, memSize);
    //    endmethod
    //`endif

        method Action perfReq(PerfLocation loc, PerfType t);
            perfReqQ.enq(ProcPerfReq {
                loc: loc,
                pType: t
            });
        endmethod
    endinterface

    interface CoreIndInv coreIndInv;
        method ActionValue#(Bit#(64)) to_host = toGet(toHostQ).get;

        method debug_verify = commitStage.debug_verify;

        method ActionValue#(ProcPerfResp) perfResp;
`ifdef PERF_COUNT
            perfRespQ.deq;
            return perfRespQ.first;
`else
            perfReqQ.deq;
            let r = perfReqQ.first;
            return ProcPerfResp {
                loc: r.loc,
                pType: r.pType,
                data: 0
            };
`endif
        endmethod

        method terminate = csrf.terminate;
    endinterface

    interface CoreIPI ipi;
        method Action recvIPI;
            ipiInQ.enq(?);
        endmethod
        method ActionValue#(CoreId) sendIPI = toGet(ipiOutQ).get;
    endinterface

    interface dCacheToParent = dMem.to_parent;
    interface iCacheToParent = iMem.to_parent;

    interface tlbToMem = l2Tlb.toMem;

    // deadlock check
    interface CoreDeadlock deadlock;
        interface dCacheCRqStuck = dMem.cRqStuck;
        interface dCachePRqStuck = dMem.pRqStuck;
        interface iCacheCRqStuck = iMem.cRqStuck;
        interface iCachePRqStuck = iMem.pRqStuck;
        interface renameInstStuck = renameStage.renameInstStuck;
        interface renameCorrectPathStuck = renameStage.renameCorrectPathStuck;
        interface commitInstStuck = commitStage.commitInstStuck;
        interface commitUserInstStuck = commitStage.commitUserInstStuck;
`ifdef CHECK_DEADLOCK
        method Action setCheckStartInstNum(Data n);
            deadlockCheckStartInstNum <= Valid (n);
        endmethod
        interface checkStarted = toGet(deadlockCheckStartedQ);
`else
        method Action setCheckStartInstNum(Data n);
            noAction;
        endmethod
        interface checkStarted = nullGet;
`endif
    endinterface

    // rename debug
    interface CoreRenameDebug renameDebug;
        interface renameErr = commitStage.renameErr;
    endinterface
endmodule

