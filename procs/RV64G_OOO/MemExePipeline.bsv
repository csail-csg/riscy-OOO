`include "ProcConfig.bsv"
import Vector::*;
import BuildVector::*;
import GetPut::*;
import ClientServer::*;
import Cntrs::*;
import Fifo::*;
import Types::*;
import ProcTypes::*;
import MemoryTypes::*;
import SynthParam::*;
import Exec::*;
import Performance::*;
import ReservationStationEhr::*;
import ReservationStationMem::*;
import ReorderBuffer::*;
import TlbTypes::*;
import DTlb::*;
import SpecLSQ::*;
import StoreBuffer::*;
import HasSpecBits::*;
import SpecFifo::*;
import SpecPoisonFifo::*;
import CCTypes::*;
import L1CoCache::*;
import Bypass::*;

typedef struct {
    // inst info
    MemFunc mem_func;
    ImmData imm;
    PhyRegs regs;
    InstTag tag;
    LdStQTag ldstq_tag;
    // speculation
    Maybe#(SpecTag) spec_tag;
} MemDispatchToRegRead deriving(Bits, Eq, FShow);

typedef struct {
    // inst info
    MemFunc mem_func;
    ImmData imm;
    InstTag tag;
    LdStQTag ldstq_tag;
    // src reg vals
    Data rVal1;
    Data rVal2;
    // speculation
    Maybe#(SpecTag) spec_tag;
} MemRegReadToExe deriving(Bits, Eq, FShow);

typedef struct {
    // inst info
    MemFunc mem_func;
    InstTag tag;
    LdStQTag ldstq_tag;
    // result
    ByteEn shiftedBE;
    Data shiftedData; // st/sc/amo data
`ifdef VERIFICATION_PACKETS
    Data origData; // unshifted data just for verification packet
`endif
    Addr vaddr; // virtual addr
    // speculation
    Maybe#(SpecTag) spec_tag;
} MemExeToFinish deriving(Bits, Eq, FShow);

// synthesized pipeline fifos
typedef SpecFifo_SB_deq_enq_C_deq_enq#(1, MemDispatchToRegRead) MemDispToRegFifo;
(* synthesize *)
module mkMemDispToRegFifo(MemDispToRegFifo);
    let m <- mkSpecFifo_SB_deq_enq_C_deq_enq(False);
    return m;
endmodule

typedef SpecFifo_SB_deq_enq_C_deq_enq#(1, MemRegReadToExe) MemRegToExeFifo;
(* synthesize *)
module mkMemRegToExeFifo(MemRegToExeFifo);
    let m <- mkSpecFifo_SB_deq_enq_C_deq_enq(False);
    return m;
endmodule

typedef SpecPoisonFifo#(`BOOKKEEPING_MEM_SIZE, MemExeToFinish) MemExeToFinFifo;
(* synthesize *)
module mkMemExeToFinFifo(MemExeToFinFifo);
    let m <- mkSpecPoisonFifo(True); // do lazy enq
    return m;
endmodule

interface MemExeInput;
    // conservative scoreboard check in reg read stage
    method RegsReady sbCons_lazyLookup(PhyRegs r);
    // Phys reg file
    method Data rf_rd1(PhyRIndx rindx);
    method Data rf_rd2(PhyRIndx rindx);
    // CSR file
    method Data csrf_rd(CSR csr);
    // ROB
    method Action rob_setExecuted_doFinishMem(InstTag t, Data data, Addr vaddr, Maybe#(Exception) cause, RobInstState new_state);
    method Action rob_setExecuted_deqLSQ(InstTag t, Data res, RobInstState new_state);
    method Action rob_setLdSpecBit(InstTag t, SpecTag ldSpecTag);

    // incr epoch without redirection (trap happens)
    method Action incrementEpochWithoutRedirect;

    // global broadcase methods
    // set aggressive sb & wake up inst in pipelines that recv bypass 
    method Action setRegReadyAggr_cache(PhyRIndx dst);
    method Action setRegReadyAggr_forward(PhyRIndx dst);
    // write reg file & set both conservative and aggressive sb & wake up inst
    method Action writeRegFile_Ld(PhyRIndx dst, Data data);
    method Action writeRegFile_LrScAmo(PhyRIndx dst, Data data);
    // redirect
    method Action redirect_action(Addr trap_pc, Maybe#(SpecTag) spec_tag, InstTag inst_tag);
    // spec update
    method Action correctSpec_doFinishMem(SpecTag t);
    method Action correctSpec_deqLSQ(SpecTag t);
    method Action incorrectSpec(SpecTag spec_tag, InstTag inst_tag);

    // performance
    method Bool doStats;
endinterface

interface MemExePipeline;
    // recv bypass from exe and finish stages of each ALU pipeline
    interface Vector#(TMul#(2, AluExeNum), RecvBypass) recvBypass;
    interface ReservationStationMem rsMemIfc;
    interface DTlb dTlbIfc;
    interface SpecLSQ lsqIfc;
    interface StoreBuffer stbIfc;
    interface DCoCache dMemIfc;
    interface SpeculationUpdate specUpdate;
    method Data getPerf(ExeStagePerfType t);
endinterface

module mkMemExePipeline#(MemExeInput inIfc)(MemExePipeline);
    Bool verbose = True;

    // reservation station
    ReservationStationMem rsMem <- mkReservationStationMem;

    // pipeline fifos
    let dispToRegQ <- mkMemDispToRegFifo;
    let regToExeQ <- mkMemRegToExeFifo;
    let exeToFinQ <- mkMemExeToFinFifo;

    // wire to recv bypass
    Vector#(TMul#(2, AluExeNum), RWire#(Tuple2#(PhyRIndx, Data))) bypassWire <- replicateM(mkRWire);

    // TLB
    DTlb dTlb <- mkDTlb;

    // store buffer
    StoreBuffer stb <- mkStoreBufferEhr;
    // LSQ
    SpecLSQ lsq <- mkSpecLSQ;
    // wire to issue Ld which just finish addr tranlation
    RWire#(LSQIssueInfo) issueLd <- mkRWire;

    // watiing bit for Lr/Sc/Amo resp
    Reg#(Bool) waitLrScAmoResp <- mkReg(False);
    // fifo for req mem
    Fifo#(1, Tuple2#(LdStQTag, Addr)) reqLdQ <- mkBypassFifo;
    Fifo#(1, ProcRq#(DProcReqId)) reqLrScAmoQ <- mkBypassFifo;
    Fifo#(1, Tuple2#(SBIndex, Addr)) reqStQ <- mkBypassFifo;
    // fifo for load result
    Fifo#(1, Tuple2#(LdStQTag, MemResp)) forwardQ <- mkBypassFifo;
    Fifo#(1, Tuple2#(LdStQTag, MemResp)) memRespLdQ <- mkBypassFifo;
    Fifo#(2, Tuple2#(LdStQTag, MemResp)) respLdQ <- mkCFFifo;
    // fifo for Lr/Sc/Amo resp
    Fifo#(2, MemResp) respLrScAmoQ <- mkCFFifo;
    // resp ifc to D$
    function Action setReadyAggr(LdStQTag t);
    action
        // Ld/Lr/Sc/Amo returns from cache, wakeup RS and set SB aggressively
        // this is done only when the resp is not wrong path
        LSQHitInfo info <- lsq.getHit(t);
        if(info.dst matches tagged Valid .dst &&& !info.waitWPResp) begin
            inIfc.setRegReadyAggr_cache(dst.indx);
        end
    endaction
    endfunction
    L1ProcResp#(DProcReqId) procRespIfc = (interface L1ProcResp;
        method Action respLd(DProcReqId id, Data d);
            LdStQTag tag = truncate(id);
            memRespLdQ.enq(tuple2(tag, d));
            // early wake up RS and set SB
            setReadyAggr(tag);
        endmethod
        method Action respLrScAmo(DProcReqId id, Data d);
            respLrScAmoQ.enq(d);
            // early wake up RS and set SB
            setReadyAggr(truncate(id));
        endmethod
        method ActionValue#(Tuple2#(LineByteEn, Line)) respSt(DProcReqId id);
            SBIndex idx = truncate(id);
            let e <- stb.deq(idx); // deq SB
            lsq.wakeupLdStalledBySB(idx); // wake up loads
            if(verbose) $fdisplay(stdout, "  [Store resp] idx = %x, ", idx, fshow(e));
            return tuple2(e.byteEn, unpack(e.data)); // return SB entry
        endmethod
    endinterface);
    // non-blocking coherent D$
    DCoCache dMem <- mkDCoCache(procRespIfc);

`ifdef PERF_COUNT
    // load mispeculation
    Count#(Data) exeKillLdCnt <- mkCount(0);
    // address translate exception
    Count#(Data) exeTlbExcepCnt <- mkCount(0);
`endif

    //=======================================================
    // Reservation Station Stuff
    //=======================================================

    rule doDispatchMem;
        rsMem.doDispatch;
        let x = rsMem.dispatchData;
        if(verbose) $display("[doDispatchMem] ", fshow(x));

        // go to next stage
        dispToRegQ.enq(ToSpecFifo {
            data: MemDispatchToRegRead {
                mem_func: x.data.mem_func,
                imm: x.data.imm,
                regs: x.regs,
                tag: x.tag,
                ldstq_tag: x.data.ldstq_tag,
                spec_tag: x.spec_tag
            },
            spec_bits: x.spec_bits
        });
    endrule

    rule doRegReadMem;
        dispToRegQ.deq;
        let dispToReg = dispToRegQ.first;
        let x = dispToReg.data;
        if(verbose) $display("[doRegReadMem] ", fshow(dispToReg));

        // check conservative scoreboard
        let regsReady = inIfc.sbCons_lazyLookup(x.regs);

        // get rVal1 (check bypass)
        Data rVal1 = ?;
        if(x.regs.src1 matches tagged Valid .src1) begin
            rVal1 <- readRFBypass(src1, regsReady.src1, inIfc.rf_rd1(src1), bypassWire);
        end

        // get rVal2 (check bypass)
        Data rVal2 = ?;
        if(x.regs.src2 matches tagged Valid .src2) begin
            rVal2 <- readRFBypass(src2, regsReady.src2, inIfc.rf_rd2(src2), bypassWire);
        end

        // go to next stage
        regToExeQ.enq(ToSpecFifo {
            data: MemRegReadToExe {
                mem_func: x.mem_func,
                imm: x.imm,
                tag: x.tag,
                ldstq_tag: x.ldstq_tag,
                rVal1: rVal1,
                rVal2: rVal2,
                spec_tag: x.spec_tag
            },
            spec_bits: dispToReg.spec_bits
        });
    endrule

    rule doExeMem;
        regToExeQ.deq;
        let regToExe = regToExeQ.first;
        let x = regToExe.data;
        if(verbose) $display("[doExeMem] ", fshow(regToExe));

        // get virtual addr & St/Sc/Amo data
        Addr vaddr = x.rVal1 + signExtend(x.imm);
        Data data = x.rVal2;

        // send to TLB
        dTlb.procReq(TlbReq{
            addr: vaddr,
            write: (case(x.mem_func)
                        St, Sc, Amo: True;
                        default: False;
                    endcase)
        });

        // get shifted data and BE
        // we can use virtual addr to shift, since page size > dword size
        function Tuple2#(ByteEn, Data) getShiftedBEData(Addr addr, ByteEn be, Data d);
            Bit#(TLog#(NumBytes)) byteOffset = truncate(addr);
            return tuple2(unpack(pack(be) << byteOffset), d << {byteOffset, 3'b0});
        endfunction
        let {shiftBE, shiftData} = getShiftedBEData(vaddr, lsq.getOrigBE(x.ldstq_tag), data);

        // go to next stage
        exeToFinQ.enq(ToSpecFifo {
            data: MemExeToFinish {
                mem_func: x.mem_func,
                tag: x.tag,
                ldstq_tag: x.ldstq_tag,
                shiftedBE: shiftBE,
                shiftedData: x.mem_func == Amo ? data : shiftData, // XXX don't shift for AMO
`ifdef VERIFICATION_PACKETS
                origData: data,
`endif
                vaddr: vaddr,
                spec_tag: x.spec_tag
            },
            spec_bits: regToExe.spec_bits
        });
    endrule

    rule doFinishMem(!exeToFinQ.first_poisoned);
        exeToFinQ.deq;
        let exeToFin = exeToFinQ.first_data;
        let x = exeToFin.data;
        if(verbose) $display("[doFinishMem] ", fshow(exeToFin));

        // [sizhuo] use value method of TLB to update full_result
        // this allows us to split if statement later in this rule
        let {paddr, cause} = dTlb.procResp;
        dTlb.deqProcResp;

        if(verbose) $display("  [doFinishMem - dTlb response] paddr %8x", paddr);
        if(isValid(cause) && verbose) $display("  [doFinishMem - dTlb response] PAGEFAULT!");

        // check whether the mem inst is a Ld
        Bool isLd = x.mem_func == Ld;

        // st/sc/amo data for verification packet
`ifdef VERIFICATION_PACKETS
        Data origData = x.origData;
`else
        Data origData = x.shiftedData;
`endif

        // [sizhuo] mem inst should always has spec tag
        SpecTag memSpecTag = validValue(x.spec_tag);
        doAssert(isValid(x.spec_tag), "mem inst must have spec tag");

        // update LSQ & ROB
        (* split *)
        if (isValid(cause)) (* nosplit *) begin
            // LSQ entry should be killed due to exception in addr translation
            // ROB entry becomes Executed to handle exception
            inIfc.rob_setExecuted_doFinishMem(x.tag, origData, x.vaddr, cause, Executed);
            // use spec bits to kill other entries, but wait until ROB commit to resolve exception
            inIfc.incorrectSpec(memSpecTag, x.tag);
            inIfc.incrementEpochWithoutRedirect;
`ifdef PERF_COUNT
            // performance counter
            if(inIfc.doStats) begin
                exeTlbExcepCnt.incr(1);
            end
`endif
        end
        else (* nosplit *) begin
            // no exception in addr translation
            // LSQ entry is updated with addr/data
            // for Ld, we make SpecBits of LSQ entry to depend on itself
            LSQUpdateResult updRes <- lsq.update(
                x.ldstq_tag, paddr, x.shiftedBE, x.shiftedData, isLd ? Valid (memSpecTag) : Invalid
            );
            if(isLd) begin
                if(verbose) $display("  [doFinishMem - Ld update result] ", fshow(updRes));
                if(!updRes.waitWPResp) begin
                    // Ld entry is not waiting for wrong path inst
                    // so we try to eagerly issue it **ONLY** this cycle
                    // because this Ld cannot be enq into the issueQ of LSQ this cycle
                    // but it may get into issueQ in later cycles
                    issueLd.wset(LSQIssueInfo {
                        tag: x.ldstq_tag,
                        paddr: paddr,
                        shiftedBE: x.shiftedBE
                    });
                end
            end
            // change ROB entry & spec bit
            inIfc.rob_setExecuted_doFinishMem(x.tag, origData, x.vaddr, cause, InLdStQ);
            if(isLd) begin
                // for Ld, we make SpecBits of ROB entry to depend on itself, and don't release spec tag
                inIfc.rob_setLdSpecBit(x.tag, memSpecTag);
            end
            else begin
                // for non-Ld, simply release spec tag
                inIfc.correctSpec_doFinishMem(memSpecTag);
            end
        end
    endrule

    rule killPoisonedInstMem(exeToFinQ.first_poisoned);
        exeToFinQ.deq;
        let exeToFin = exeToFinQ.first_data;
        if(verbose) $display("[killPoisonedInstMem] ", fshow(exeToFin));
        // drain wrong path TLB resp
        dTlb.deqProcResp;
    endrule

    //=======================================================
    // End of Reservation Station Stuff
    //=======================================================

    //=======================================================
    // Load/Store Queue Stuff
    //=======================================================

    // LSQ ordering: getKillEntry < deq,first < update < issue < respLd, wakeupLdStalledBySB < enq < correctSpec
    // SB ordering: issue < enq < search < deq

    // kill load
    rule doKillLd;
        // get load to kill from LSQ
        LSQKillInfo en <- lsq.getKillLd;
        inIfc.redirect_action(en.pc, en.ldSpecTag, en.inst_tag);
        if(verbose) $display("[doKillLd] ", fshow(en));
        // check specTag valid
        doAssert(isValid(en.ldSpecTag), "killed Ld must have spec tag");
`ifdef PERF_COUNT
        // performance counter
        if(inIfc.doStats) begin
            exeKillLdCnt.incr(1);
        end
`endif
    endrule

    // send Ld to forward or memory
    function Action doIssueLd(LSQIssueInfo info, Bool fromIssueQ);
    action
        // search SB
        SBSearchRes sbRes = stb.search(info.paddr, info.shiftedBE);
        // search LSQ
        LSQIssueResult issRes <- lsq.issue(info.tag, info.paddr, info.shiftedBE, sbRes);
        if(verbose) begin
            $display("[doIssueLd] fromIssueQ: ", fshow(fromIssueQ), " ; ",
                     fshow(info), " ; ", fshow(sbRes), " ; ", fshow(issRes));
        end
        // summarize
        if(issRes matches tagged Forward .forward) begin
            forwardQ.enq(tuple2(info.tag, forward.data));
            // early wake up
            if(forward.dst matches tagged Valid .dst) begin
                inIfc.setRegReadyAggr_forward(dst.indx);
            end
        end
        else if(issRes == ToCache) begin
            reqLdQ.enq(tuple2(zeroExtend(info.tag), info.paddr));
        end
        else begin
            doAssert(issRes == Stall, "load is stalled");
        end
    endaction
    endfunction

    rule doIssueLdFromIssueQ;
        // get issue entry from LSQ
        LSQIssueInfo info <- lsq.getIssueLd;
        doIssueLd(info, True);
    endrule

    (* descending_urgency = "doIssueLdFromIssueQ, doIssueLdFromUpdate" *) // prioritize older load
    rule doIssueLdFromUpdate(issueLd.wget matches tagged Valid .info);
        // issue the entry that just updates LSQ this cycle
        doIssueLd(info, False);
    endrule

    function Action memInstSetRobEx(InstTag inst_tag, Data data);
    action
        inIfc.rob_setExecuted_deqLSQ(inst_tag, data, Executed);
    endaction
    endfunction

    // handle load resp
    rule doRespLd;
        let {t, d} <- toGet(respLdQ).get;
        LSQRespLdResult res <- lsq.respLd(t, d);
        if(verbose) $display("[doRespLd] ", fshow(t), "; ", fshow(d), "; ", fshow(res));
        if(res.dst matches tagged Valid .dst) begin
            inIfc.writeRegFile_Ld(dst.indx, res.data);
        end
        if(res.wrongPath) begin
            doAssert(res.dst == Invalid, "wrong path resp cannot write reg");
        end
    endrule

    // deq LSQ
    LSQDeqEntry lsqDeqEn = lsq.first;

    // we can deq Ld when
    // (1) spec bit only depend on itself
    // (2) Done but not killed
    rule doDeqLSQ_Ld(
        lsqDeqEn.mem_inst.mem_func == Ld &&
        lsqDeqEn.spec_bits == (1 << validValue(lsqDeqEn.ldSpecTag)) &&
        lsqDeqEn.state == Done && !lsqDeqEn.ldKilled
    );
        lsq.deq;
        // release spec tag
        inIfc.correctSpec_deqLSQ(validValue(lsqDeqEn.ldSpecTag));
        // set ROB as Executed
        memInstSetRobEx(lsqDeqEn.inst_tag, lsqDeqEn.shiftedData);
        if(verbose) $display("[doDeqLSQ_Ld] ", fshow(lsqDeqEn));
    endrule

    // we can deq St when (1) computed (2) no spec bit (3) can send to SB
    rule doDeqLSQ_St(
        lsqDeqEn.mem_inst.mem_func == St &&&
        lsqDeqEn.spec_bits == 0 &&& lsqDeqEn.computed &&&
        stb.getEnqIndex(lsqDeqEn.paddr) matches tagged Valid .sbIdx
    );
        lsq.deq;
        // send to SB
        stb.enq(sbIdx, lsqDeqEn.paddr, lsqDeqEn.shiftedBE, lsqDeqEn.shiftedData);
        // set ROB as Executed (St has no data to write reg)
        memInstSetRobEx(lsqDeqEn.inst_tag, 0);
        if(verbose) $display("[doDeqLSQ_St] ", fshow(lsqDeqEn));
    endrule

    // for Lr/Sc/Amo we first need to execution
    Bool isDeqLrScAmo = (case(lsqDeqEn.mem_inst.mem_func)
        Lr, Sc, Amo: return True;
        default: return False;
    endcase);

    function AmoInst toAmoInst(MemInst x) =
        AmoInst{func       : x.amo_func,
                doubleWord : x.byteEn == replicate(True),
                aq         : x.aq,
                rl         : x.rl};

    // issue Lr/Sc/Amo when
    // (1) not waiting for Lr/Sc/Amo resp
    // (2) addr/data is ready (no need to check state which must be Idle)
    // (3) not pending on wrong path resp
    // (4) no spec bit
    // (5) SB does not match that addr
    rule doDeqLSQ_LrScAmo_issue(
        isDeqLrScAmo && !waitLrScAmoResp && !lsqDeqEn.waitWPResp &&
        lsqDeqEn.computed && lsqDeqEn.spec_bits == 0 &&
        stb.noMatch(lsqDeqEn.paddr, lsqDeqEn.shiftedBE)
    );
        // set wait bit
        waitLrScAmoResp <= True;
        // send to mem
        ProcRq#(DProcReqId) req = ProcRq {
            id: zeroExtend(lsqDeqEn.ldstq_tag),
            addr: lsqDeqEn.paddr,
            toState: lsqDeqEn.mem_inst.mem_func == Lr ? S : M,
            op: (case(lsqDeqEn.mem_inst.mem_func)
                Lr: return Lr;
                Sc: return Sc;
                Amo: return Amo;
                default: return ?;
            endcase),
            // XXX Amo uses **original** data (lsq.first.shiftedData is the original data for Amo)
            // AMO doesn't use BE
            // Sc uses **shifted** BE and data, Lr doesn't care about BE or data
            byteEn: lsqDeqEn.shiftedBE,
            data: lsqDeqEn.shiftedData,
            amoInst: toAmoInst(lsqDeqEn.mem_inst)
        };
        reqLrScAmoQ.enq(req);
        if(verbose) $display("[doDeqLSQ_LrScAmo_issue] ", fshow(lsqDeqEn), "; ", fshow(req));
    endrule

    // deq Lr/Sc/Amo from LSQ when resp comes
    rule doDeqLSQ_LrScAmo_deq(isDeqLrScAmo && waitLrScAmoResp);
        // deq LSQ & reset wait bit
        lsq.deq;
        waitLrScAmoResp <= False;
        // get resp data and may need shifting
        let d <- toGet(respLrScAmoQ).get;
        Data resp = (case(lsqDeqEn.mem_inst.mem_func)
            Lr: return gatherLoad(lsqDeqEn.paddr, lsqDeqEn.mem_inst.byteEn, lsqDeqEn.mem_inst.unsignedLd, d); 
            Sc: return d;
            Amo: return d;
            default: return ?;
        endcase);
        // write reg file & set ROB as Executed
        if(lsqDeqEn.dst matches tagged Valid .dst) begin
            inIfc.writeRegFile_LrScAmo(dst.indx, resp);
        end
        memInstSetRobEx(lsqDeqEn.inst_tag, resp);
        if(verbose) $display("[doDeqLSQ_LrScAmo_deq] ", fshow(lsqDeqEn), "; ", fshow(d), "; ", fshow(resp));
    endrule

    // send store to mem
    rule doIssueSB;
        let {sbIdx, en} <- stb.issue;
        reqStQ.enq(tuple2(sbIdx, {en.addr, 0}));
    endrule

    // merge ld resp into 1 FIFO
    rule mergeForwardResp;
        forwardQ.deq;
        respLdQ.enq(forwardQ.first);
    endrule
    rule mergeMemLdResp;
        memRespLdQ.deq;
        respLdQ.enq(memRespLdQ.first);
    endrule

    // send req to D$
    rule sendLdToMem;
        let {lsqTag, addr} <- toGet(reqLdQ).get;
        dMem.procReq.req(ProcRq {
            id: zeroExtend(lsqTag),
            addr: addr,
            toState: S,
            op: Ld,
            byteEn: ?,
            data: ?,
            amoInst: ?
        });
    endrule
    (* descending_urgency = "sendLdToMem, sendStToMem" *) // prioritize Ld over St
    rule sendStToMem;
        let {sbIdx, addr} <- toGet(reqStQ).get;
        dMem.procReq.req(ProcRq {
            id: zeroExtend(sbIdx),
            addr: addr,
            toState: M,
            op: St,
            byteEn: ?,
            data: ?,
            amoInst: ?
        });
    endrule
    (* descending_urgency = "sendLrScAmoToMem, sendStToMem" *) // prioritize Lr/Sc/Amo over St
    rule sendLrScAmoToMem;
        let r <- toGet(reqLrScAmoQ).get;
        dMem.procReq.req(r);
        // tag doesn't matter for Lr/Sc/Amo, they are lsq.first
    endrule

    //=======================================================
    // End of Load/Store Queue Stuff
    //=======================================================

    interface recvBypass = map(getRecvBypassIfc, bypassWire);
    interface rsMemIfc = rsMem;
    interface dTlbIfc = dTlb;
    interface lsqIfc = lsq;
    interface stbIfc = stb;
    interface dMemIfc = dMem;
    interface specUpdate = joinSpeculationUpdate(vec(
        rsMem.specUpdate,
        dispToRegQ.specUpdate,
        regToExeQ.specUpdate,
        exeToFinQ.specUpdate,
        lsq.specUpdate
    ));
    method Data getPerf(ExeStagePerfType t);
        return (case(t)
`ifdef PERF_COUNT
            ExeKillLd: exeKillLdCnt;
            ExeTlbExcep: exeTlbExcepCnt;
`endif
            default: 0;
        endcase);
    endmethod
endmodule
