
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
import FIFO::*;
import Fifo::*;
import Ehr::*;
import Connectable::*;

import Types::*;
import ProcTypes::*;
import MemoryTypes::*;
import CacheUtils::*;
import TlbTypes::*;
import VerificationPacket::*;
import Performance::*;
import Decode::*;
import Exec::*;
import ITlb::*;
import DTlb::*;
import L2Tlb::*;
import TlbConnect::*;
import CsrFile::*;
import ArchRegFile::*;
import Fpu::*;
import MulDiv::*;
import StoreBuffer::*;
import CCTypes::*;
import L1CoCache::*;
import L1Bank::*;
import IBank::*;

// for legacy debug types
import FetchStage::*;
import RenameStage::*;
import CommitStage::*;

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

// stages for an inst
typedef enum {
    Off,
    FetchITlb_RecvHtif,
    FetchICache,
    Decode,
    RegRead,
    Execute,
    ExeFpu,
    ExeMulDiv,
    ExeTlbResp,
    ExeMemResp,
    Commit,
    CheckInterrupt_SendHtif
} Stage deriving(Bits, Eq, FShow);

(* synthesize *)
module mkCore#(CoreId coreId)(Core);
    let verbose = True;

    Reg#(Stage) stage <- mkReg(Off);

    // processor local states
    Reg#(Maybe#(Exception)) exception <- mkReg(Invalid);
    Reg#(DecodedInst) decInst <- mkReg(unpack(0));
    Reg#(ArchRegs) srcDstRegs <- mkReg(unpack(0));
    Reg#(Maybe#(Data)) src1Val <- mkReg(Invalid);
    Reg#(Maybe#(Data)) src2Val <- mkReg(Invalid);
    Reg#(Maybe#(Data)) src3Val <- mkReg(Invalid);
    Reg#(Maybe#(Data)) srcCsrVal <- mkReg(Invalid);
    Reg#(Addr) predPc <- mkReg(0); // we defer update of PC until commit stage
    Reg#(Addr) memVAddr <- mkReg(0); // virtual address of ld/st page fault
    Reg#(Data) csrData <- mkReg(0); // csr dst data for CSRXXX inst (update csr at commit)
    Reg#(Tuple2#(LineByteEn, Line)) stBeData <- mkReg(unpack(0)); // normal store BE & data
    Reg#(Tuple2#(ByteEn, Data)) scBeData <- mkReg(unpack(0)); // store cond BE & data
    Reg#(Data) amoData <- mkReg(0); // AMO data (BE is not used)
    Reg#(Bool) willDirtyFpu <- mkReg(False); // used for CSR update in commit
    Reg#(Bit#(5)) fflags <- mkReg(0); // flags for FP execution, used to update FPU CSR at commit

    function Action resetLocalState;
    action
        exception <= Invalid;
        decInst <= unpack(0);
        srcDstRegs <= unpack(0);
        src1Val <= Invalid;
        src2Val <= Invalid;
        src3Val <= Invalid;
        srcCsrVal <= Invalid;
        predPc <= 0;
        memVAddr <= 0;
        csrData <= 0;
        stBeData <= unpack(0);
        scBeData <= unpack(0);
        amoData <= 0;
        willDirtyFpu <= False;
        fflags <= 0;
    endaction
    endfunction

    // arch regs
    Reg#(Addr) pcReg <- mkReg(0);
    ArchRegFile rf <- mkArchRegFile;
    CsrFile csrf <- mkCsrFileWithId(zeroExtend(coreId)); // hartid in CSRF should be core id

    // TLBs
    ITlb iTlb <- mkITlb;
    DTlb dTlb <- mkDTlb;
    L2Tlb l2Tlb <- mkL2Tlb;
    mkTlbConnect(iTlb.toParent, dTlb.toParent, l2Tlb.toChildren);

    // exe units
    MulDivExec mulDivExec <- mkBoothRoughMulDivExec;
    FpuExec fpuExec <- mkFpuExecPipeline;
    StoreBuffer stb <- mkStoreBufferEhr;

    // non-blocking coherent I$ and D$
    ICoCache iMem <- mkICoCache;
    FIFO#(Data) memRespQ <- mkFIFO;
    L1ProcResp#(DProcReqId) procRespIfc = (interface L1ProcResp;
        method Action respLd(DProcReqId id, Data d);
            memRespQ.enq(d);
        endmethod
        method Action respLrScAmo(DProcReqId id, Data d);
            memRespQ.enq(d);
        endmethod
        method ActionValue#(Tuple2#(LineByteEn, Line)) respSt(DProcReqId id);
            memRespQ.enq(0);
            return stBeData;
        endmethod
    endinterface);
    DCoCache dMem <- mkDCoCache(procRespIfc);

    // flags to flush
    Reg#(Bool)  flush_tlbs <- mkReg(False);
    Reg#(Bool)  update_vm_info <- mkReg(False);
    Reg#(Bool)  flush_reservation <- mkReg(False);

    // HTIF
    Reg#(Bool) htifStall <- mkReg(False);
    FIFO#(Data) toHostQ <- mkFIFO1;
    Fifo#(2, Data) fromHostQ <- mkCFFifo;

    // performance (dummy)
    FIFO#(ProcPerfReq) perfReqQ <- mkFIFO1;

`ifdef CHECK_DEADLOCK
    // when to start deadlock checking
    Reg#(Bool) startDeadlockCheck <- mkReg(False);
    Reg#(Maybe#(Data)) deadlockCheckStartInstNum <- mkReg(Invalid); // check start after committing these number of inst
    FIFO#(void) deadlockCheckStartedQ <- mkFIFO;

    rule doStartDeadlockCheck(
        deadlockCheckStartInstNum matches tagged Valid .n &&&
        n < csrf.rd(CSRinstret) &&& !startDeadlockCheck &&& stage != Off
    );
        startDeadlockCheck <= True;
        deadlockCheckStartedQ.enq(?);
    endrule

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
        return CommitStuck {
            pc: pcReg,
            iType: decInst.iType,
            trap: exception matches tagged Valid .e ? Valid (Exception (e)) : Invalid,
            state: unpack(0),
            specBits: unpack(0),
            specTag: unpack(0),
            stbEmpty: True,
            prv: csrf.csrState.prv,
            htifStall: htifStall
        };
    endfunction

    (* fire_when_enabled *)
    rule checkDeadlock_commitInst(startDeadlockCheck && commitInstTimer == maxBound);
        commitInstStuckQ.enq(commitStuck);
        commitInstStuckSent.send;
    endrule

    (* fire_when_enabled *)
    rule checkDeadlock_commitUserInst(startDeadlockCheck && commitUserInstTimer == maxBound);
        commitUserInstStuckQ.enq(commitStuck);
        commitUserInstStuckSent.send;
    endrule

    (* fire_when_enabled, no_implicit_conditions *)
    rule incrDeadlockTimer(startDeadlockCheck);
        function DeadlockTimer getNextTimer(DeadlockTimer t);
            return t == maxBound ? maxBound : t + 1;
        endfunction
        commitInstTimer <= (commitInst || commitInstStuckSent) ? 0 : getNextTimer(commitInstTimer);
        commitUserInstTimer <= (commitUserInst || commitUserInstStuckSent) ? 0 : getNextTimer(commitUserInstTimer);
    endrule
`endif

    rule doFetchITlb(stage == FetchITlb_RecvHtif && 
                     // wait htif done
                     !htifStall &&
                     // wait flush to be done
                     !flush_reservation && !flush_tlbs && !update_vm_info &&
                     iTlb.flush_done && dTlb.flush_done);
        iTlb.to_proc.request.put(pcReg);
        predPc <= pcReg + 4; // use PC+4 here, for branches we overwrite this later
        stage <= FetchICache;
        if(verbose) $display("[doFetchITlb] PC %x", pcReg);
    endrule

    rule doFetchICache(stage == FetchICache);
        let {phy_pc, cause} <- iTlb.to_proc.response.get;
        if(verbose) $display("[doFetchICache] phy PC %x, exception ", phy_pc, fshow(cause));
        if(!isValid(cause)) begin
            // PC translation succeeds, req I$
            iMem.to_proc.request.put(phy_pc);
            stage <= Decode;
        end
        else begin
            // PC translation faults, go to commit to take exception
            exception <= cause;
            stage <= Commit;
        end
    endrule

    rule doDecode(stage == Decode);
        // we only do 1 inst
        Vector#(SupSize, Maybe#(Instruction)) insts <- iMem.to_proc.response.get;
        if(insts[0] matches tagged Valid .inst) begin
            let decRes = decode(inst);
            if(decRes.illegalInst) begin
                // illegal inst, go to commit to take exception
                exception <= Valid (IllegalInst);
                stage <= Commit;
            end
            else begin
                // decode succeeds, record decode results, go to reg read
                decInst <= decRes.dInst;
                srcDstRegs <= decRes.regs;
                stage <= RegRead;
            end
            if(verbose) $display("[doDecode] inst %x, decRes ", inst, fshow(decRes));
        end
        else begin
            doAssert(False, "Must at least get 1 inst");
        end
    endrule

    rule doRegRead(stage == RegRead);
        // first check for new exceptions after decode
        let newExcep = checkForException(decInst, srcDstRegs, csrf.csrState);
        if(isValid(newExcep)) begin
            exception <= newExcep;
            stage <= Commit;
            if(verbose) $display("[doRegRead] exception ", fshow(newExcep));
        end
        else begin
            // no more exception, so we check if the inst needs execution
            if(decInst.execFunc == Other) begin
                // no need to execute, go to commit
                stage <= Commit;
                if(verbose) $display("[doRegRead] no execution needed");
            end
            else begin
                // need to execute, read reg
                if(srcDstRegs.src1 matches tagged Valid .idx) begin
                    src1Val <= Valid (rf.rd(idx));
                end
                if(srcDstRegs.src2 matches tagged Valid .idx) begin
                    src2Val <= Valid (rf.rd(idx));
                end
                if(srcDstRegs.src3 matches tagged Valid .idx) begin
                    src3Val <= Valid (rf.rd(Fpu (idx)));
                end
                if(decInst.csr matches tagged Valid .csr) begin
                    srcCsrVal <= Valid (csrf.rd(csr));
                end
                // check if FPU will be modified
                if(srcDstRegs.dst matches tagged Valid (tagged Fpu .idx)) begin
                    willDirtyFpu <= True;
                end
                // go to execute this inst
                stage <= Execute;
                if(verbose) $display("[doRegRead] proceed to execute");
            end
        end
    endrule
    
    rule doExecute(stage == Execute);
        // source reg values
        if(verbose) begin
            $display(
                "[doExecute] src1Val ", fshow(src1Val),
                ", src2Val ", fshow(src2Val),
                ", src3Val ", fshow(src3Val),
                ", srcCsrVal ", fshow(srcCsrVal)
            );
        end
        // execute depends on inst type
        case(decInst.execFunc) matches
            tagged Mem .mem: begin
                Data rVal1 = fromMaybe(0, src1Val);
                Data rVal2 = fromMaybe(0, src2Val);
                // compute VA, and start translation
                Addr vaddr = rVal1 + signExtend(fromMaybe(0, decInst.imm));
                dTlb.procReq(TlbReq {
                    addr: vaddr,
                    write: (case(mem.mem_func)
                                St, Sc, Amo: True;
                                default: False;
                            endcase)
                });
                memVAddr <= vaddr; // save VA in case of page fault
                // compute st/sc/amo data & BE
                case(mem.mem_func)
                    St, Sc: begin
                        Bit#(TLog#(NumBytes)) byteOffset = truncate(vaddr);
                        ByteEn shiftedBE = unpack(pack(mem.byteEn) << byteOffset);
                        Data shiftedData = rVal2 << {byteOffset, 3'b0};
                        if(mem.mem_func == Sc) begin
                            scBeData <= tuple2(shiftedBE, shiftedData);
                        end
                        else begin
                            LineDataOffset dataOffset = getLineDataOffset(vaddr);
                            Vector#(LineSzData, ByteEn) stBE = unpack(0);
                            Vector#(LineSzData, Data) stData = unpack(0);
                            stBE[dataOffset] = shiftedBE;
                            stData[dataOffset] = shiftedData;
                            stBeData <= tuple2(unpack(pack(stBE)), stData);
                        end
                    end
                    Amo: begin
                        amoData <= rVal2;
                    end
                endcase
                // wait TLB resp
                stage <= ExeTlbResp;
                $display("[doExecute - Mem] VA %x", vaddr);
            end
            tagged MulDiv .muldiv: begin
                Data rVal1 = fromMaybe(0, src1Val);
                Data rVal2 = fromMaybe(0, src2Val);
                mulDivExec.exec(muldiv, rVal1, rVal2);
                stage <= ExeMulDiv;
            end
            tagged Fpu .fpu: begin
                Data rVal1 = fromMaybe(0, src1Val);
                Data rVal2 = fromMaybe(0, src2Val);
                Data rVal3 = 0; // FIXME FMA is not implemented
                fpuExec.exec(fpu, rVal1, rVal2, rVal3);
                stage <= ExeFpu;
            end
            default: begin
                // ALU may use CSR as src operand
                Data rVal1 = isValid(decInst.csr) ? fromMaybe(0, srcCsrVal) : fromMaybe(0, src1Val);
                Data rVal2 = fromMaybe(0, src2Val);
                // ALU/Branch/Jump
                ExecResult execRes = basicExec(decInst, rVal1, rVal2, pcReg, predPc);
                // write arch reg & save results
                predPc <= execRes.controlFlow.nextPc; // PC change defer to commit
                if(srcDstRegs.dst matches tagged Valid .idx) begin
                    rf.wr(idx, execRes.data);
                end
                if(isValid(decInst.csr)) begin
                    csrData <= execRes.csrData;
                end
                stage <= Commit;

                if(verbose) begin
                    $display(
                        "[doExecute - AluBr] predPc %x, dstVal ",
                        execRes.controlFlow.nextPc,
                        fshow(isValid(srcDstRegs.dst) ? Valid (execRes.data) : Invalid),
                        ", csrData ",
                        fshow(isValid(decInst.csr) ? Valid (execRes.csrData) : Invalid)
                    );
                end
            end
        endcase
    endrule

    rule doExeFpu(stage == ExeFpu);
        fpuExec.result_deq;
        let res = fpuExec.result_data;
        if(srcDstRegs.dst matches tagged Valid .idx) begin
            rf.wr(idx, res.data);
        end
        fflags <= res.fflags;
        stage <= Commit;

        if(verbose) begin
            $display(
                "[doExeFpu] fflags %x, dstVal ", res.fflags,
                fshow(isValid(srcDstRegs.dst) ? Valid (res.data) : Invalid)
            );
        end
    endrule

    rule doExeMulDiv(stage == ExeMulDiv);
        mulDivExec.result_deq;
        let res = mulDivExec.result_data;
        if(srcDstRegs.dst matches tagged Valid .idx) begin
            rf.wr(idx, res);
        end
        stage <= Commit;

        if(verbose) begin
            $display(
                "[doExeMulDiv] dstVal ",
                fshow(isValid(srcDstRegs.dst) ? Valid (res) : Invalid)
            );
        end
    endrule

    rule doExeTlbResp(stage == ExeTlbResp);
        // get TLB resp
        let {paddr, cause} = dTlb.procResp;
        dTlb.deqProcResp;
        // get mem inst
        MemInst mem = unpack(0);
        if(decInst.execFunc matches tagged Mem .mi) begin
            mem = mi;
        end
        else begin
            doAssert(False, "Must be mem inst");
        end
        // check for page fault
        if(isValid(cause)) begin
            exception <= cause;
            stage <= Commit;
            if(verbose) $display("[doExeTlbResp] exception ", fshow(cause));
        end
        else begin
            // successfull translation, now issue to D$
            ProcRq#(DProcReqId) r = ProcRq {
                id: 0,
                addr: paddr,
                toState: (case(mem.mem_func)
                    Ld, Lr: return S;
                    default: return M;
                endcase),
                op: (case(mem.mem_func)
                    Ld: return Ld;
                    St: return St;
                    Lr: return Lr;
                    Sc: return Sc;
                    Amo: return Amo;
                    default: return ?;
                endcase),
                // BE only matters for Sc, it use shifted BE
                byteEn: mem.mem_func == Sc ? tpl_1(scBeData) : unpack(0),
                // Sc use shifted data, Amo use original data
                data: (case(mem.mem_func)
                    Sc: return tpl_2(scBeData);
                    Amo: return amoData;
                    default: return 0;
                endcase),
                amoInst: (mem.mem_func == Amo ? (AmoInst {
                    func: mem.amo_func,
                    doubleWord: mem.byteEn == replicate(True),
                    aq: mem.aq,
                    rl: mem.rl
                }) : unpack(0))
            };
            dMem.procReq.req(r);
            stage <= ExeMemResp;
            if(verbose) $display("[doExeTlbResp] paddr %x, mem req ", paddr, fshow(r));
        end
    endrule

    rule doExeMemResp(stage == ExeMemResp);
        // get mem inst
        MemInst mem = unpack(0);
        if(decInst.execFunc matches tagged Mem .mi) begin
            mem = mi;
        end
        else begin
            doAssert(False, "Must be mem inst");
        end
        // get mem resp
        memRespQ.deq;
        Data resp = memRespQ.first;
        Data resultData = (case(mem.mem_func)
            Ld, Lr: return gatherLoad(memVAddr, mem.byteEn, mem.unsignedLd, resp);
            Sc, Amo: return resp;
            default: return 0;
        endcase);
        if(srcDstRegs.dst matches tagged Valid .idx) begin
            rf.wr(idx, resultData);
        end
        stage <= Commit;

        if(verbose) begin
            $display(
                "[doExeMemResp] resp %x, dstVal ", resp,
                fshow(isValid(srcDstRegs.dst) ? Valid (resultData) : Invalid)
            );
        end
    endrule

    Bool tlbNoPending = iTlb.noPendingReq &&
                        dTlb.noPendingReqOrWrite &&
                        l2Tlb.noPendingReqOrWrite;

    rule doCommit(stage == Commit);
        Addr next_pc;
        if(exception matches tagged Valid .cause) begin
            // exception: flush & stalls
            when(tlbNoPending, noAction);
            update_vm_info <= True;
            flush_reservation <= True;
            // trap handling and redirection
            next_pc <- csrf.trap(Exception (cause), pcReg, memVAddr);
        end
        else begin
            // not execption, this inst can commit
            // flush & stalls for system inst
            if(isSystem(decInst.iType)) begin
                if(decInst.iType == SFence) begin
                    flush_tlbs <= True;
                end
                if(decInst.iType == Fence) begin
                    noAction; // nothing special for Fence now
                end
                when(tlbNoPending, noAction);
                update_vm_info <= True;
                flush_reservation <= True;
            end
            // commit this inst
            if(decInst.iType == Sret) begin
                next_pc <- csrf.sret;
            end
            else if(decInst.iType == Mrts) begin
                next_pc <- csrf.mrts;
            end
            else begin
                // normal inst: see if CSR needs update
                csrf.wr(decInst.csr, csrData, fflags, willDirtyFpu, False);
                next_pc = predPc;
            end
        end
        // reset all bookkeepings, redirect pc, go to check interrupt
        resetLocalState;
        csrf.instret_inc(1); // increment inst count
        pcReg <= next_pc;
        stage <= CheckInterrupt_SendHtif;
        
        if(verbose) $display("[doCommit] exception ", fshow(exception), ", next pc %x", next_pc);

`ifdef CHECK_DEADLOCK
        commitInst.send; // ROB head is removed
        if(csrf.csrState.prv == 0) begin
            commitUserInst.send;
        end
`endif
    endrule

    rule doInterrupt(stage == CheckInterrupt_SendHtif);
        if(csrf.pending_interrupt matches tagged Valid .cause) begin
            // interrupt: flush & stalls
            when(tlbNoPending, noAction);
            update_vm_info <= True;
            flush_reservation <= True;
            // trap handling and redirection
            let next_pc <- csrf.trap(Interrupt (cause), pcReg, 0);
            pcReg <= next_pc;
        end
        stage <= FetchITlb_RecvHtif;
    endrule
    
    // send to host msg: make sure that we stall the processor before fetching
    // next inst
    rule csrfToHost(stage == CheckInterrupt_SendHtif);
        let ret <- csrf.csrfToHost;
        if(verbose) $display("[csrfToHost] core %d, val %x", coreId, ret);
        // Don't stall for reading from the terminal
        // Thomas says this is awful
        // [sizhuo] do not compare lower bits, they may be garbage
        if (truncateLSB(ret) != 16'h0100) begin
            htifStall <= True;
        end
        toHostQ.enq(ret);
    endrule

    // try to process fromhost msg, only do this when mfromhost CSR is 0
    // and we are not processing other msg (e.g. other IPI/fromhost)
    // XXX since software may do read-modify-write on CSR, any change to CSR by
    // external world should only happen when there is no inst in flight. We
    // choose the fetch stage, which is also the stage when processor is
    // stalled by HTIF
    rule doFromHost(stage == FetchITlb_RecvHtif && csrf.fromHostZero);
        fromHostQ.deq;
        csrf.hostToCsrf(fromHostQ.first);
        htifStall <= False;
        if(verbose) $display("[doFromHost] core %d, val %x", coreId, fromHostQ.first);
    endrule

    // preempt has 2 functions here
    // 1. break scheduling cycles
    // 2. XXX since csrf is configReg now, we should not let this rule fire
    // together with doCommit or doInterrupt because we read csrf here and
    // write csrf in doCommit
    (* preempts = "prepareCachesAndTlbs, doCommit" *)
    (* preempts = "prepareCachesAndTlbs, doInterrupt" *)
    rule prepareCachesAndTlbs(flush_reservation || flush_tlbs || update_vm_info);
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

    interface CoreReq coreReq;
        method Action start(
            Bit#(64) startpc, Bool ipi_wait_msip_zero,
            Bit#(64) verification_packets_to_ignore,
            Bool send_synchronization_packets
        ) if(stage == Off);
            pcReg <= startpc;
            stage <= FetchITlb_RecvHtif;
            csrf.hostToCsrf(0);
        endmethod

        method Action from_host(Bit#(64) v);
            fromHostQ.enq(v);
        endmethod

        method Action perfReq(PerfLocation loc, PerfType t);
            perfReqQ.enq(ProcPerfReq {
                loc: loc,
                pType: t
            });
        endmethod
    endinterface

    interface CoreIndInv coreIndInv;
        method ActionValue#(Bit#(64)) to_host = toGet(toHostQ).get;

        method ActionValue#(VerificationPacket) debug_verify if(False);
            return ?;
        endmethod

        method ActionValue#(ProcPerfResp) perfResp;
            perfReqQ.deq;
            let r = perfReqQ.first;
            return ProcPerfResp {
                loc: r.loc,
                pType: r.pType,
                data: 0
            };
        endmethod

        method terminate = csrf.terminate;
    endinterface

    interface CoreIPI ipi;
        method Action recvIPI;
            doAssert(False, "IPI not implemented");
        endmethod
        method ActionValue#(CoreId) sendIPI if(False);
            return 0;
        endmethod
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
        interface renameInstStuck = nullGet;
        interface renameCorrectPathStuck = nullGet;
`ifdef CHECK_DEADLOCK
        interface commitInstStuck = toGet(commitInstStuckQ);
        interface commitUserInstStuck = toGet(commitUserInstStuckQ);
        method Action setCheckStartInstNum(Data n);
            deadlockCheckStartInstNum <= Valid (n);
        endmethod
        interface checkStarted = toGet(deadlockCheckStartedQ);
`else
        interface commitInstStuck = nullGet;
        interface commitUserInstStuck = nullGet;
        method Action setCheckStartInstNum(Data n);
            noAction;
        endmethod
        interface checkStarted = nullGet;
`endif
    endinterface

    // rename debug
    interface CoreRenameDebug renameDebug;
        interface renameErr = nullGet;
    endinterface
endmodule

