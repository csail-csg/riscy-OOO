import Ehr::*;
import GetPut::*;
import Fifo::*;
import RegFile::*;
import Vector::*;
import BuildVector::*;
import FShow::*;
import Randomizable::*;
import ConfigReg::*;
import StmtFSM::*;

import Types::*;
import MemoryTypes::*;
import ProcTypes::*;
import CCTypes::*;
import TestTypes::*;
import Exec::*;
import SpecFifo::*;
import SplitLSQ::*;
import StoreBuffer::*;
import RefMem::*;
import CCM::*;
import DelayTLB::*;
import SpecTagManager::*;
import HasSpecBits::*;
import GSpecUpdate::*;

// FIFO to begin memory pipeline, needed for an extra stage to update LSQ data
typedef struct {
    TestId testId;
    LdStQTag lsqTag;
    SpecTag specTag;
} MemInput deriving(Bits, Eq, FShow);

typedef SpecFifo_SB_deq_enq_C_deq_enq#(1, MemInput) MemInputQ;
(* synthesize *)
module mkMemInputQ(MemInputQ);
    let m <- mkSpecFifo_SB_deq_enq_C_deq_enq(False);
    return m;
endmodule

typedef enum {Invalid, Lr, Sc} WaitBlockingResp deriving(Bits, Eq, FShow);

typedef struct {
    LineDataOffset offset;
    ByteEn shiftedBE;
    Data shiftedData;
} WaitStResp deriving(Bits, Eq, FShow);

(* synthesize *)
module mkTbIntegrate(Empty);
    // reference & test req generator
    RefMem refMem <- mkRefMem;
    RegFile#(TestId, TestEntry) test <- mkRegFileFull;
    // id of next test of the same type (i.e. same Ld/St type). If id ==
    // TestNum, then no more next test.
    RegFile#(TestId, TestCnt) nextPtr <- mkRegFileFull;

    Randomize#(LineAddr) randLineAddr <- mkConstrainedRandomizer(0, fromInteger(valueof(TestLineNum) - 1));
    Randomize#(LineDataOffset) randDataSel <- mkConstrainedRandomizer(0, fromInteger(valueof(TestDataSelNum) - 1));
    Randomize#(ByteOffset) randByteOff <- mkGenericRandomizer;
    Randomize#(AccessRange) randRange <- mkGenericRandomizer;
    //Randomize#(TestMemFunc) randFunc <- mkGenericRandomizer;
    Randomize#(TestMemFunc) randFunc <- mkConstrainedRandomizer(Ld, St);
    Randomize#(Data) randData <- mkGenericRandomizer;

    // test state & counters
    Reg#(TestState) state <- mkReg(StartInit);
    Reg#(LineAddr) testLineAddr <- mkReg(0);
    Reg#(TestCnt) genPtr <- mkReg(0);
    Reg#(Maybe#(TestId)) lastLdPtr <- mkReg(Invalid);
    Reg#(Maybe#(TestId)) lastStPtr <- mkReg(Invalid);
    Reg#(TestCnt) ldCnt <- mkReg(0);
    Reg#(TestCnt) stCnt <- mkReg(0);
    Reg#(TestCnt) lrCnt <- mkReg(0);
    Reg#(TestCnt) scCnt <- mkReg(0);
    Reg#(Bit#(64)) killCnt <- mkConfigReg(0);
    Reg#(Bit#(64)) stallCnt <- mkConfigReg(0);

    // logs
    Reg#(File) reqLog <- mkReg(InvalidFile);
    Reg#(File) respLdLog <- mkReg(InvalidFile);
    Reg#(File) respStLog <- mkReg(InvalidFile);
    Reg#(File) checkLog <- mkReg(InvalidFile);

    // detect deadlock
    PulseWire commitInst <- mkPulseWire;
    PulseWire deqSB <- mkPulseWire;
    Reg#(TimeOut) timeOut <- mkReg(0);

    // detect test done
    Wire#(Bool) stbEmpty_for_done <- mkBypassWire;
    Wire#(TestCnt) comLdPtr_for_done <- mkBypassWire;
    Wire#(TestCnt) comStPtr_for_done <- mkBypassWire;

    // DUT
    SpecTagManager stm <- mkSpecTagManager;
    MemInputQ inputQ <- mkMemInputQ;
    DelayTLB tlb <- mkDelayTLB;
    SplitLSQ lsq <- mkSplitLSQ;
`ifdef TSO_MM
    StoreBuffer stb <- mkDummyStoreBuffer;
`else
    StoreBuffer stb <- mkStoreBufferEhr;
`endif
    // for Lr/Sc we first need to execution
    Reg#(WaitBlockingResp) waitLrScResp <- mkReg(Invalid);
`ifdef TSO_MM
    Fifo#(1, WaitStResp) waitStRespQ <- mkCFFifo;
`endif
    // req to send to LSQ
    Reg#(TestCnt) sendPtr <- mkReg(0);
    // req to commit
    Reg#(TestCnt) comLdPtr <- mkReg(fromInteger(valueof(TestNum)));
    Reg#(TestCnt) comStPtr <- mkReg(fromInteger(valueof(TestNum)));
    // Test IDs for in-flight loads
    Vector#(LdQSize, Reg#(TestId)) testingLdPtr <- replicateM(mkRegU);
    // load resps
    RegFile#(TestId, Data) ldResp <- mkRegFileFull;
    // FIFOs to req CCM
    Fifo#(1, Tuple2#(CCMReqId, MemReq)) reqLdQ <- mkBypassFifo;
    Fifo#(1, Tuple2#(CCMReqId, MemReq)) reqStQ <- mkBypassFifo;
    Fifo#(1, Tuple2#(CCMReqId, MemReq)) reqLrScQ <- mkBypassFifo;

    RWire#(LSQIssueLdInfo) issueLd <- mkRWire; // issue right after TLB resp

    // FIFOs to hold load result
    Fifo#(2, Data) respLrScQ <- mkCFFifo;
    Fifo#(2, Tuple2#(LdQTag, Data)) forwardQ <- mkCFFifo;
    Fifo#(2, Tuple2#(LdQTag, Data)) memRespLdQ <- mkCFFifo;

    // memory
    CCMProcResp memRespIfc = (interface CCMProcResp;
        method Action respLd(CCMReqId id, Data d);
            memRespLdQ.enq(tuple2(id, d));
        endmethod

        method Action respLrScAmo(Data d);
            respLrScQ.enq(d);
        endmethod

        method ActionValue#(Tuple2#(LineByteEn, Line)) respSt(CCMReqId id);
`ifdef TSO_MM
            lsq.deqSt;
            let waitSt <- toGet(waitStRespQ).get;
            Vector#(LineSzData, ByteEn) be = replicate(replicate(False));
            Line data = replicate(0);
            be[waitSt.offset] = waitSt.shiftedBE;
            data[waitSt.offset] = waitSt.shiftedData;
            // incr comptr & set wire to reset timeout
            comStPtr <= nextPtr.sub(comStPtr);
            commitInst.send;
            $display("[respSt] %t: comPtr %d; ", $time, comStPtr, fshow(waitSt));
            return tuple2(unpack(pack(be)), data);
`else
            SBIndex idx = truncate(id);
            // deq SB
            let e <- stb.deq(idx);
            // wake up loads in LSQ
            lsq.wakeupLdStalledBySB(idx);
            // send wire to reset timeout
            deqSB.send;
            // return SB entry
            return tuple2(e.byteEn, unpack(e.data));
`endif
        endmethod
    endinterface);
    CCM ccm <- mkCCM(memRespIfc);

    // speculation related
    GSpecUpdate#(2) globalSpecUpdate <- mkGSpecUpdate(joinSpeculationUpdate(vec(
        stm.specUpdate,
        tlb.specUpdate,
        lsq.specUpdate,
        inputQ.specUpdate
    )));
    Integer tlbCorrectSpecPort = 0;
    Integer lsqCorrectSpecPort = 1;

    function Action redirect(TestId newSendPtr, SpecTag specTag);
    action
        sendPtr <= zeroExtend(newSendPtr);
        globalSpecUpdate.incorrectSpec(specTag);
    endaction
    endfunction

    // =============== Test FSM ===============

    // FSM to init
    Stmt initSeq = (seq
        // init mem
        repeat(fromInteger(valueof(TestLineNum)))
        action
            refMem.initLine({testLineAddr, 0}, replicate(0));
            ccm.initLine({testLineAddr, 0}, replicate(0));
            testLineAddr <= testLineAddr + 1;
        endaction

        // reset test addr & init randomizers & open files
        action
            refMem.initDone;
            ccm.initDone;
            randLineAddr.cntrl.init;
            randDataSel.cntrl.init;
            randByteOff.cntrl.init;
            randRange.cntrl.init;
            randFunc.cntrl.init;
            randData.cntrl.init;
            let f <- $fopen("req.log");
            reqLog <= f;
            f <- $fopen("respLd.log");
            respLdLog <= f;
            f <- $fopen("respSt.log");
            respStLog <= f;
            f <- $fopen("check.log");
            checkLog <= f;
        endaction
        $fdisplay(stderr, "INFO: init mem done");

        // generate req
        while(genPtr < fromInteger(valueof(TestNum)))
        action
            // randomize req
            let lineAddr <- randLineAddr.next;
            let dataSel <- randDataSel.next;
            let offset <- randByteOff.next;
            let range <- randRange.next;
            let func <- randFunc.next;
            let {paddr, memInst} = getAddrMemInst(lineAddr, dataSel, offset, range, func);
            let stData <- randData.next;
            // get resp
            Data resp = ?;
            case(func)
                Ld, Lr: begin
                    let r <- refMem.procReq(MemReq {
                        op: toMemOp(func),
                        addr: paddr,
                        byteEn: replicate(False),
                        data: ?
                    });
                    resp = gatherLoad(paddr, memInst.byteEn, memInst.unsignedLd, r);
                end
                St, Sc: begin
                    let {be, d} = scatterStore(paddr, memInst.byteEn, stData);
                    // directly record Sc resp
                    resp <- refMem.procReq(MemReq {
                        op: toMemOp(func),
                        addr: paddr,
                        byteEn: be,
                        data: d
                    });
                end
                default: doAssert(False, "unsupport func");
            endcase
            // record test entry
            let entry = TestEntry {
                memInst: memInst,
                paddr: paddr,
                stData: stData,
                resp: resp
            };
            test.upd(truncate(genPtr), entry);
            // update genPtr
            genPtr <= genPtr + 1;
            // udpate next id & record first Ld/St ptr into comLd/StPtr
            if(func == Ld || func == Lr) begin
                if(lastLdPtr matches tagged Valid .id) begin
                    nextPtr.upd(id, genPtr);
                end
                lastLdPtr <= Valid (truncate(genPtr));
                if(comLdPtr == fromInteger(valueof(TestNum))) begin
                    comLdPtr <= genPtr;
                end
            end
            else begin
                if(lastStPtr matches tagged Valid .id) begin
                    nextPtr.upd(id, genPtr);
                end
                lastStPtr <= Valid (truncate(genPtr));
                if(comStPtr == fromInteger(valueof(TestNum))) begin
                    comStPtr <= genPtr;
                end
            end
            // log
            $fdisplay(reqLog, "%d: ", genPtr, fshow(entry));
            // stats
            case(func)
                Ld: ldCnt <= ldCnt + 1;
                Lr: lrCnt <= lrCnt + 1;
                St: stCnt <= stCnt + 1;
                Sc: scCnt <= scCnt + 1;
            endcase
        endaction
        // fill in nextPtr for the last Ld and St
        action
            if(lastLdPtr matches tagged Valid .id) begin
                nextPtr.upd(id, fromInteger(valueof(TestNum)));
            end
        endaction
        action
            if(lastStPtr matches tagged Valid .id) begin
                nextPtr.upd(id, fromInteger(valueof(TestNum)));
            end
        endaction
        $fdisplay(stderr, "INFO: gen req done");
    endseq);

    FSM initFSM <- mkFSM(initSeq);

    rule doStartInit(state == StartInit);
        initFSM.start;
        state <= WaitInitDone;
    endrule

    rule doWaitInitDone(state == WaitInitDone);
        initFSM.waitTillDone;
        state <= Testing;
    endrule

    // wait for test to be done and check time out
    (* fire_when_enabled, no_implicit_conditions *)
    rule setForDone;
        stbEmpty_for_done <= stb.isEmpty;
        comLdPtr_for_done <= comStPtr;
        comStPtr_for_done <= comLdPtr;
    endrule

    rule waitTestDone(state == Testing &&
                      comLdPtr_for_done == fromInteger(valueof(TestNum)) &&
                      comStPtr_for_done == fromInteger(valueof(TestNum)) &&
                      stbEmpty_for_done);
        state <= StartCheck;
        $fdisplay(stderr, "INFO: test done");
    endrule

    (* fire_when_enabled, no_implicit_conditions *)
    rule doTimeOut(state == Testing);
        if(commitInst || deqSB) begin
            timeOut <= 0;
        end
        else begin
            timeOut <= timeOut + 1;
        end
        if(timeOut >= fromInteger(valueof(MaxTimeOut))) begin
            doAssert(False, "time out");
        end
    endrule

    // final check FSM
    Stmt checkSeq = (seq
        // check mem value
        testLineAddr <= 0;
        repeat(fromInteger(valueof(TestNum)))
        action
            Addr a = {testLineAddr, 0};
            let refLine = refMem.getLine(a);
            let ccmLine = ccm.getLine(a);
            $fdisplay(checkLog, "%x: ", a, fshow(refLine), "; ", fshow(ccmLine));
            doAssert(refLine == ccmLine, "wrong mem val");
            // change state
            testLineAddr <= testLineAddr + 1;
        endaction
        $fdisplay(stderr, "INFO: check mem done");

        // print pass & stats
        $fdisplay(stderr, "INFO: pass");
        $fdisplay(stderr, "stats: Ld %d, Lr %d, St %d, Sc %d, kill %d, stall %d",
            ldCnt, lrCnt, stCnt, scCnt, killCnt, stallCnt
        );
        $finish;
    endseq);

    FSM checkFSM <- mkFSM(checkSeq);

    rule doStartCheck(state == StartCheck);
        checkFSM.start;
        state <= WaitCheckDone;
    endrule

    // ========== Logic to make LSQ run =============

    // send tests to mem pipeline (sendPtr may be redirected due to
    // mis-speculation, so we may resend request many times)
    rule sendReq(state == Testing && sendPtr < fromInteger(valueof(TestNum)));
        // get test req
        TestId testId = truncate(sendPtr);
        TestEntry en = test.sub(testId);

        // claim spec tag
        SpecTag specTag = stm.nextSpecTag;
        stm.claimSpecTag;
        // get spec bits: depend on itself
        SpecBits specBits = stm.currentSpecBits | (1 << specTag);

        // send to LSQ (assume dst reg is 0)
        LdStQTag lsqTag;
        let mem_func = en.memInst.mem_func;
        if(isLdQMemFunc(mem_func)) begin
            Maybe#(PhyDst) dst = Valid (PhyDst {indx: 0, isFpuReg: False});
            lsq.enqLd(toInstTag(testId), en.memInst, dst, specBits);
            lsqTag = validValue(lsq.enqLdTag);
            doAssert(isValid(lsq.enqLdTag), "must be valid tag");
            // record test id for this new inflight load
            if(lsqTag matches tagged Ld .t) begin
                testingLdPtr[t] <= testId;
            end
            else begin
                doAssert(False, "must be Ld tag");
            end
        end
        else begin
            Maybe#(PhyDst) dst = mem_func == St ? Invalid : Valid (PhyDst {indx: 0, isFpuReg: False});
            lsq.enqSt(toInstTag(testId), en.memInst, dst, specBits);
            lsqTag = validValue(lsq.enqStTag);
            doAssert(isValid(lsq.enqStTag), "must be valid tag");
        end

        // send to inputQ
        inputQ.enq(ToSpecFifo {
            data: MemInput {
                testId: testId,
                lsqTag: lsqTag,
                specTag: specTag
            },
            spec_bits: specBits
        });

        // increment ptr
        sendPtr <= sendPtr + 1;
        $display("[doEnqLSQ] %t: sendPtr %d; ", $time, sendPtr, fshow(en),
            "; specTag %d; specBits %d; lsqTag %d", specTag, specBits, lsqTag
        );
    endrule

    function Tuple2#(ByteEn, Data) getShiftedBEData(Addr addr, ByteEn be, Data d);
        Bit#(TLog#(NumBytes)) byteOffset = truncate(addr);
        return tuple2(unpack(pack(be) << byteOffset), d << {byteOffset, 3'b0});
    endfunction

    // req to TLB & udpate data
    rule doTlbReq;
        inputQ.deq;
        MemInput in = inputQ.first.data;
        SpecBits specBits = inputQ.first.spec_bits;
        // get shifted BE & data
        let testEn = test.sub(in.testId);
        let {shift_be, shift_data} = getShiftedBEData(
            testEn.paddr, testEn.memInst.byteEn, testEn.stData
        );
        // update data for St/Sc
        if(in.lsqTag matches tagged St .t) begin
            lsq.updateData(t, shift_data);
        end
        // req to TLB
        let r = DelayTLBReq {
            testId: in.testId,
            lsqTag: in.lsqTag,
            specTag: in.specTag,
            shiftedBE: shift_be
        };
        tlb.procReq(r, specBits);
        $display("[doTlbReq] %t: ", $time, fshow(inputQ.first),
            "; ", fshow(testEn), "; ", fshow(shift_data), "; ", fshow(r)
        );
    endrule

    // update LSQ with phy addr
    rule doTlbResp;
        tlb.deqResp;
        DelayTLBReq r = tlb.respVal;
        // get addr & data
        TestEntry testEn = test.sub(r.testId);
        Bool isLd = testEn.memInst.mem_func == Ld;
        // print
        $display("[doTlbResp] %t: ", $time, fshow(r), "; ", fshow(testEn));
        // upate LSQ
        Bool isMMIO = False; // not MMIO in this test
        let res <- lsq.updateAddr(
            r.lsqTag, testEn.paddr, isMMIO,
            r.shiftedBE, isLd ? Valid (r.specTag) : Invalid
        );
        if(isLd && !res.waitWPResp) begin
            LdQTag ldTag = ?;
            if(r.lsqTag matches tagged Ld .t) begin
                ldTag = t;
            end
            else begin
                doAssert(False, "must be Ld");
            end
            issueLd.wset(LSQIssueLdInfo {
                tag: ldTag,
                paddr: testEn.paddr,
                shiftedBE: r.shiftedBE
            });
        end
        // free spec tag except for Ld
        if(!isLd) begin
            globalSpecUpdate.correctSpec[tlbCorrectSpecPort].put(r.specTag);
        end
    endrule

    // kill Ld
    function Action doKillLd(LSQKillLdInfo info, String rule_name);
    action
        redirect(info.instTag.t, validValue(info.specTag));
        // stat
        killCnt <= killCnt + 1;
        // print
        $display(rule_name, " %t: ", $time, fshow(info));
        // check specTag valid
        doAssert(isValid(info.specTag), "killed Ld must have spec tag");
    endaction
    endfunction
        
    rule doKillLdByLdSt;
        let info <- lsq.getLdKilledByLdSt;
        doKillLd(info, "[doKillLdByLdSt]");
    endrule

`ifdef TSO_MM
    rule doKillByCache;
        let info <- lsq.getLdKilledByCache;
        doKillLd(info, "[doKillLdByCache]");
    endrule
`endif

    // issue Ld
    function Action doIssueLd(LSQIssueLdInfo info, String rule_name);
    action
        // search SB
        SBSearchRes sbRes = stb.search(info.paddr, info.shiftedBE);
        // search LSQ
        let issRes <- lsq.issueLd(info.tag, info.paddr, info.shiftedBE, sbRes);
        $display(rule_name, " %t: ", $time, "; ", fshow(info),
            "; ", fshow(sbRes), "; ", fshow(issRes)
        );
        // summarize
        if(issRes matches tagged Forward .forward) begin
            forwardQ.enq(tuple2(info.tag, forward.data));
        end
        else if(issRes == ToCache) begin
            reqLdQ.enq(tuple2(zeroExtend(info.tag), MemReq {
                op: Ld,
                addr: info.paddr,
                byteEn: replicate(False),
                data: ?
            }));
        end
        else begin
            doAssert(issRes == Stall, "load is stalled");
            stallCnt <= stallCnt + 1;
        end
    endaction
    endfunction

    rule doIssueLdFromIssueQ;
        // get issue entry from LSQ
        let info <- lsq.getIssueLd;
        doIssueLd(info, "[doIssueLdFromIssueQ]");
    endrule

    (* descending_urgency = "doIssueLdFromIssueQ, doIssueLdFromUpdate" *)
    rule doIssueLdFromUpdate(issueLd.wget matches tagged Valid .info);
        // issue the entry that just updates LSQ this cycle
        doIssueLd(info, "[doIssueLdFromUpdate]");
    endrule

    // get Ld resp
    function Action doRespLd(LdQTag t, Data d, String rule_name);
    action
        TestId testId = testingLdPtr[t];
        let res <- lsq.respLd(t, d);
        $display(rule_name, " %t: ", $time, fshow(testId), "; ",
            fshow(t), "; ", fshow(d), "; ", fshow(res)
        );
        // record Ld resp
        if(isValid(res.dst) && !res.wrongPath) begin
            ldResp.upd(testId, res.data);
        end
    endaction
    endfunction

    rule doRespLdFromMem;
        let {t, d} <- toGet(memRespLdQ).get;
        doRespLd(t, d, "[doRespLdFromMem]");
    endrule

    (* descending_urgency = "doRespLdFromMem, doRespLdFromForward" *)
    rule doRespLdFromForward;
        let {t, d} <- toGet(forwardQ).get;
        doRespLd(t, d, "[doRespFromForward]");
    endrule

    // deq Ld/Lr
    let lsqDeqLd = lsq.firstLd;

    rule doDeqLSQ_Ld(lsqDeqLd.memFunc == Ld);
        lsq.deqLd;
        Data resp = ldResp.sub(lsqDeqLd.instTag.t);
        TestEntry testEn = test.sub(truncate(comLdPtr));
        // log
        $display("[doDeqLSQ_Ld] %t: comPtr %d; ", $time, comLdPtr,
            fshow(lsqDeqLd), "; ", fshow(resp), "; ", fshow(testEn)
        );
        $fdisplay(respLdLog, "%d: ", comLdPtr, fshow(lsqDeqLd), "; ", fshow(resp));
        // release spec tag
        globalSpecUpdate.correctSpec[lsqCorrectSpecPort].put(validValue(lsqDeqLd.specTag));
        // change comptr & set wire to reset timeout
        comLdPtr <= nextPtr.sub(truncate(comLdPtr));
        commitInst.send;
        // check
        doAssert(!lsqDeqLd.isMMIO, "no MMIO in test");
        doAssert(isValid(lsqDeqLd.specTag), "must have spec tag");
        doAssert(lsqDeqLd.specBits == (1 << validValue(lsqDeqLd.specTag)),
                 "spec bits = 1 << spec tag");
        doAssert(comLdPtr != fromInteger(valueof(TestNum)), "comLdPtr overflow");
        doAssert(comLdPtr == zeroExtend(lsqDeqLd.instTag.t), "comLdPtr != inst_tag");
        doAssert(testEn.resp == resp, "wrong Ld resp");
    endrule

    rule doDeqLSQ_Lr_issue(
        lsqDeqLd.memFunc == Lr
        && waitLrScResp == Invalid
        && lsqDeqLd.specBits == 0
`ifndef TSO_MM
        && stb.noMatchLdQ(lsqDeqLd.paddr, lsqDeqLd.shiftedBE)
`endif
    );
        // send to mem
        waitLrScResp <= Lr;
        reqLrScQ.enq(tuple2(0, MemReq {
            op: Lr,
            addr: lsqDeqLd.paddr,
            byteEn: lsqDeqLd.shiftedBE,
            data: ?
        }));
        // print
        $display("[doDeqLSQ_Lr_issue] %t: ", $time, fshow(lsqDeqLd));
        // check
        doAssert(!lsqDeqLd.isMMIO, "no MMIO in test");
        doAssert(!isValid(lsqDeqLd.specTag), "cannot have spec tag");
    endrule

    rule doDeqLSQ_Lr_deq(lsqDeqLd.memFunc == Lr && waitLrScResp == Lr);
        lsq.deqLd;
        waitLrScResp <= Invalid;
        let data <- toGet(respLrScQ).get;
        Data resp = gatherLoad(lsqDeqLd.paddr, lsqDeqLd.byteEn, lsqDeqLd.unsignedLd, data); 
        // incr comptr & set wire to reset timeout
        comLdPtr <= nextPtr.sub(truncate(comLdPtr));
        commitInst.send;
        // log
        TestEntry testEn = test.sub(truncate(comLdPtr));
        $display("[doDeqLSQ_Lr_deq] %t: comPtr %d; ", $time, comLdPtr,
            fshow(lsqDeqLd), "; ", fshow(resp), "; ", fshow(testEn)
        );
        $fdisplay(respLdLog, "%d: ", comLdPtr, fshow(lsqDeqLd), "; ", fshow(resp));
        // check
        doAssert(!lsqDeqLd.isMMIO, "no MMIO in test");
        doAssert(!isValid(lsqDeqLd.specTag), "cannot have spec tag");
        doAssert(lsqDeqLd.specBits == 0, "cannot have spec bits");
        doAssert(comLdPtr != fromInteger(valueof(TestNum)), "comLdPtr overflow");
        doAssert(comLdPtr == zeroExtend(lsqDeqLd.instTag.t), "comLdPtr != inst_tag");
        doAssert(resp == testEn.resp, "wrong Lr resp");
    endrule

    // deq StQ
    let lsqDeqSt = lsq.firstSt;

    // We have to put spec bits == 0 into guard, because the older load may be
    // deq in the same cycle, and spec bits is not cleared immediately (it is
    // done at the end of this cycle)
`ifdef TSO_MM
    rule doDeqLSQ_St(
        lsqDeqSt.memFunc == St &&
        lsqDeqSt.specBits == 0
    );
        // send to mem
        reqStQ.enq(tuple2(0, MemReq {
            op: St,
            addr: lsqDeqSt.paddr,
            byteEn: ?,
            data: ?
        }));
        // record waiting for store resp
        LineDataOffset offset = getLineDataOffset(lsqDeqSt.paddr);
        waitStRespQ.enq(WaitStResp {
            offset: getLineDataOffset(lsqDeqSt.paddr),
            shiftedBE: lsqDeqSt.shiftedBE,
            shiftedData: lsqDeqSt.stData
        });
        // leave deq LSQ and incr comStPtr to cache resp time
        // log
        TestEntry testEn = test.sub(truncate(comStPtr));
        $display("[doDeqLSQ_St] %t: comPtr %d; ", $time, comStPtr,
            fshow(lsqDeqSt), "; ", fshow(testEn)
        );
        $fdisplay(respStLog, "%d: ", comStPtr, fshow(lsqDeqSt));
        // check
        doAssert(!lsqDeqSt.isMMIO, "no MMIO in test");
        doAssert(lsqDeqSt.specBits == 0, "cannot have spec bits");
        doAssert(comStPtr != fromInteger(valueof(TestNum)), "comStPtr overflow");
        doAssert(comStPtr == zeroExtend(lsqDeqSt.instTag.t), "comStPtr != inst_tag");
        let {be, d} = scatterStore(testEn.paddr, testEn.memInst.byteEn, testEn.stData);
        doAssert(be == lsqDeqSt.shiftedBE, "wrong BE");
        doAssert(d == lsqDeqSt.stData, "wrong data");
    endrule

`else

    rule doDeqLSQ_St(
        lsqDeqSt.memFunc == St &&&
        lsqDeqSt.specBits == 0 &&&
        stb.getEnqIndex(lsqDeqSt.paddr) matches tagged Valid .sbIdx
    );
        lsq.deqSt;
        // send to SB
        stb.enq(sbIdx, lsqDeqSt.paddr, lsqDeqSt.shiftedBE, lsqDeqSt.stData);
        // incr comptr & set wire to reset timeout
        comStPtr <= nextPtr.sub(comStPtr);
        commitInst.send;
        // log
        TestEntry testEn = test.sub(truncate(comStPtr));
        $display("[doDeqLSQ_St] %t: comPtr %d; ", $time, comStPtr,
            fshow(lsqDeqSt), "; ", fshow(testEn)
        );
        $fdisplay(respStLog, "%d: ", comStPtr, fshow(lsqDeqSt));
        // check
        doAssert(!lsqDeqSt.isMMIO, "no MMIO in test");
        doAssert(lsqDeqSt.specBits == 0, "cannot have spec bits");
        doAssert(comStPtr != fromInteger(valueof(TestNum)), "comStPtr overflow");
        doAssert(comStPtr == zeroExtend(lsqDeqSt.instTag.t), "comStPtr != inst_tag");
        let {be, d} = scatterStore(testEn.paddr, testEn.memInst.byteEn, testEn.stData);
        doAssert(be == lsqDeqSt.shiftedBE, "wrong BE");
        doAssert(d == lsqDeqSt.stData, "wrong data");
    endrule

    rule doIssueSB;
        let {sbIdx, en} <- stb.issue;
        reqStQ.enq(tuple2(zeroExtend(sbIdx), MemReq {
            op: St,
            addr: {en.addr, 0},
            byteEn: ?,
            data: ?
        }));
    endrule
`endif

    rule doDeqLSQ_Sc_issue(
        lsqDeqSt.memFunc == Sc
        && waitLrScResp == Invalid
        && lsqDeqSt.specBits == 0
`ifndef TSO_MM
        && stb.noMatchStQ(lsqDeqSt.paddr, lsqDeqSt.shiftedBE)
`endif
    );
        // send to mem
        waitLrScResp <= Sc;
        reqLrScQ.enq(tuple2(0, MemReq {
            op: Sc,
            addr: lsqDeqSt.paddr,
            byteEn: lsqDeqSt.shiftedBE,
            data: lsqDeqSt.stData
        }));
        // print
        $display("[doDeqLSQ_Sc_issue] %t: ", $time, fshow(lsqDeqSt));
        // check
        doAssert(!lsqDeqSt.isMMIO, "no MMIO in test");
        doAssert(!isValid(lsqDeqSt.specTag), "cannot have spec tag");
    endrule

    rule doDeqLSQ_Sc_deq(lsqDeqSt.memFunc == Sc && waitLrScResp == Sc);
        lsq.deqSt;
        waitLrScResp <= Invalid;
        let resp <- toGet(respLrScQ).get;
        // incr comptr & set wire to reset timeout
        comStPtr <= nextPtr.sub(comStPtr);
        commitInst.send;
        // log
        TestEntry testEn = test.sub(truncate(comStPtr));
        $display("[doDeqLSQ_Sc_deq] %t: comPtr %d; ", $time, comStPtr,
            fshow(lsqDeqSt), "; ", fshow(resp), "; ", fshow(testEn)
        );
        $fdisplay(respStLog, "%d: ", comStPtr, fshow(lsqDeqSt), "; ", fshow(resp));
        // check result
        doAssert(!lsqDeqSt.isMMIO, "no MMIO in test");
        doAssert(!isValid(lsqDeqSt.specTag), "cannot have spec tag");
        doAssert(lsqDeqSt.specBits == 0, "cannot have spec bits");
        doAssert(comStPtr != fromInteger(valueof(TestNum)), "comStPtr overflow");
        doAssert(comStPtr == zeroExtend(lsqDeqSt.instTag.t), "comStPtr != inst_tag");
        doAssert(resp == testEn.resp, "wrong Sc resp");
        let {be, d} = scatterStore(testEn.paddr, testEn.memInst.byteEn, testEn.stData);
        doAssert(be == lsqDeqSt.shiftedBE, "wrong BE");
        doAssert(d == lsqDeqSt.stData, "wrong data");
    endrule

    rule sendLdToMem;
        let {id, r} <- toGet(reqLdQ).get;
        ccm.procReq(id, r);
    endrule

    rule sendStToMem;
        let {id, r} <- toGet(reqStQ).get;
        ccm.procReq(id, r);
    endrule

    rule sendLrScToMem;
        let {id, r} <- toGet(reqLrScQ).get;
        ccm.procReq(id, r);
    endrule
endmodule
