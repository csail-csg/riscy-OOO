import Ehr::*;
import GetPut::*;
import Fifo::*;
import RegFile::*;
import Vector::*;
import BuildVector::*;
import FShow::*;
import Randomizable::*;
import ConfigReg::*;

import Types::*;
import MemoryTypes::*;
import ProcTypes::*;
import CCTypes::*;
import TestTypes::*;
import Exec::*;
import SplitLSQ::*;
import StoreBuffer::*;
import RefMem::*;
import CCM::*;
import DelayTLB::*;
import SpecTagManager::*;
import HasSpecBits::*;
import GSpecUpdate::*;

(* synthesize *)
module mkTbLSQSB(Empty);
    // reference & test req generator
    RefMem refMem <- mkRefMem;
    RegFile#(TestId, TestEntry) test <- mkRegFileFull;

    Randomize#(CLineAddr) randCAddr <- mkConstrainedRandomizer(0, fromInteger(valueof(CLineNum) - 1));
    Randomize#(CLineDataSel) randDataSel <- mkConstrainedRandomizer(0, fromInteger(valueof(DataSelNum) - 1));
    Randomize#(ByteOffset) randByteOff <- mkGenericRandomizer;
    Randomize#(AccessRange) randRange <- mkGenericRandomizer;
    //Randomize#(TestMemFunc) randFunc <- mkGenericRandomizer;
    Randomize#(TestMemFunc) randFunc <- mkConstrainedRandomizer(Ld, St);
    Randomize#(Data) randData <- mkGenericRandomizer;

    // test state & counters
    Reg#(TestState) state <- mkReg(Init);
    Reg#(LineAddr) testLineAddr <- mkReg(0);
    Reg#(TestId) genPtr <- mkReg(0);
    Reg#(TestCnt) ldCnt <- mkReg(0);
    Reg#(TestCnt) stCnt <- mkReg(0);
    Reg#(TestCnt) lrCnt <- mkReg(0);
    Reg#(TestCnt) scCnt <- mkReg(0);
    Reg#(Bit#(64)) killCnt <- mkConfigReg(0);
    Reg#(Bit#(64)) dropRespCnt <- mkConfigReg(0);

    // logs
    Reg#(File) reqLog <- mkReg(InvalidFile);
    Reg#(File) respLog <- mkReg(InvalidFile);
    Reg#(File) checkLog <- mkReg(InvalidFile);

    // detect deadlock
    PulseWire commitInst <- mkPulseWire;
    PulseWire deqSB <- mkPulseWire;
    Reg#(TimeOut) timeOut <- mkReg(0);

    // DUT
    SpecTagManager stm <- mkSpecTagManager;
    DelayTLB tlb <- mkDelayTLB;
    SplitLSQ lsq <- mkSpecLSQ;
`ifdef TSO_MM
    StoreBuffer stb <- mkDummyStoreBuffer;
`else
    StoreBuffer stb <- mkStoreBufferEhr;
`endif
    Reg#(TestCnt) sendPtr <- mkReg(0); // req to send to LSQ
    Reg#(TestCnt) comPtr <- mkReg(0); // req to commit

    RWire#(LSQIssueInfo) issueLd <- mkRWire; // issue right after TLB resp

    // FIFOs to hold load result
    Fifo#(2, Data) respLrScQ <- mkCFFifo;
    Fifo#(2, Tuple2#(LdStQTag, Data)) forwardQ <- mkCFFifo;
    Fifo#(2, Tuple2#(LdStQTag, Data)) memRespLdQ <- mkCFFifo;

    // memory
    CCMProcResp memRespIfc = (interface CCMProcResp;
        method Action respLd(CCMReqId id, Data d);
            memRespLdQ.enq(tuple2(truncate(id), d));
        endmethod

        method Action respLrScAmo(Data d);
            respLrScQ.enq(d);
        endmethod

        method ActionValue#(Tuple2#(CLineByteEn, CacheLine)) respSt(CCMReqId id);
            SBIndex idx = truncate(id);
            // deq SB
            let e <- stb.deq(idx);
            // wake up loads in LSQ
            lsq.wakeupLdStalledBySB(idx);
            // send wire to reset timeout
            deqSB.send;
            // return SB entry
            return tuple2(e.byteEn, e.data);
        endmethod
    endinterface);
    CCM ccm <- mkCCM(memRespIfc);

    GSpecUpdate#(2) globalSpecUpdate <- mkGSpecUpdate(joinSpeculationUpdate(vec(
        stm.specUpdate,
        tlb.specUpdate,
        lsq.specUpdate
    )));
    Integer tlbCorrectSpecPort = 0;
    Integer lsqCorrectSpecPort = 1;

    function Action redirect(TestId newSendPtr, SpecTag specTag);
    action
        sendPtr <= zeroExtend(newSendPtr);
        globalSpecUpdate.incorrectSpec(specTag);
    endaction
    endfunction

    rule doInit(state == Init);
        refMem.initCLine({testCAddr, 0}, 0);
        ccm.initCLine({testCAddr, 0}, 0);
        if(testCAddr < fromInteger(valueof(CLineNum) - 1)) begin
            testCAddr <= testCAddr + 1;
        end
        else begin
            testCAddr <= 0;
            refMem.initDone;
            ccm.initDone;
            randCAddr.cntrl.init;
            randDataSel.cntrl.init;
            randByteOff.cntrl.init;
            randRange.cntrl.init;
            randFunc.cntrl.init;
            randData.cntrl.init;
            let f <- $fopen("req.log");
            reqLog <= f;
            f <- $fopen("resp.log");
            respLog <= f;
            f <- $fopen("check.log");
            checkLog <= f;
            state <= GenReq;
            $fdisplay(stderr, "INFO: init done");
        end
    endrule

    rule doGenReq(state == GenReq);
        // randomize req
        let cAddr <- randCAddr.next;
        let dataSel <- randDataSel.next;
        let offset <- randByteOff.next;
        let range <- randRange.next;
        let func <- randFunc.next;
        let {paddr, memInst} = getAddrMemInst(cAddr, dataSel, offset, range, func);
        let stData <- randData.next;
        // get resp
        Data resp = ?;
        case(func)
            Ld, Lr: begin
                let r <- refMem.procReq(MemReq {
                    op: toMemOp(func),
                    byteEn: replicate(False),
                    addr: paddr,
                    data: ?,
                    amoInst: Invalid
                });
                resp = gatherLoad(paddr, memInst.byteEn, memInst.unsignedLd, r);
            end
            St, Sc: begin
                let {be, d} = scatterStore(paddr, memInst.byteEn, stData);
                // directly record Sc resp
                resp <- refMem.procReq(MemReq {
                    op: toMemOp(func),
                    byteEn: be,
                    addr: paddr,
                    data: d,
                    amoInst: Invalid
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
        test.upd(genPtr, entry);
        $fdisplay(reqLog, "%d: ", genPtr, fshow(entry));
        // stats
        case(func)
            Ld: ldCnt <= ldCnt + 1;
            Lr: lrCnt <= lrCnt + 1;
            St: stCnt <= stCnt + 1;
            Sc: scCnt <= scCnt + 1;
        endcase
        // change state
        genPtr <= genPtr + 1;
        if(genPtr == fromInteger(valueof(TestNum) - 1)) begin
            state <= Test;
            $fdisplay(stderr, "INFO: gen req done");
        end
    endrule

    rule doEnqLSQ(state == Test && sendPtr < fromInteger(valueof(TestNum)));
        // get test req
        TestId testId = truncate(sendPtr);
        TestEntry en = test.sub(testId);
        // XXX currently only test mem inst that needs addr translation
        // claim spec tag
        SpecTag specTag = stm.nextSpecTag;
        stm.claimSpecTag;
        // get spec bits: depend on itself
        SpecBits specBits = stm.currentSpecBits | (1 << specTag);
        // send to LSQ (assume dst reg is 0)
        let lsqTag = lsq.enqTag;
        Maybe#(PhyDst) dst = en.memInst.mem_func == St ? Invalid : Valid (PhyDst {indx: 0, isFpuReg: False});
        lsq.enq(testId, 0, en.memInst, dst, specBits); // pc is not used here
        // send to TLB
        tlb.procReq(DelayTLBReq {
            testId: testId,
            lsqTag: lsqTag,
            specTag: specTag
        }, specBits);
        // increment ptr
        sendPtr <= sendPtr + 1;
        $display("[doEnqLSQ] %t: sendPtr %d; ", $time, sendPtr, fshow(en),
            "; specTag %d; specBits %d; lsqTag %d", specTag, specBits, lsqTag
        );
    endrule

    rule doTlbResp;
        tlb.deqResp;
        DelayTLBReq r = tlb.respVal;
        // get addr & data
        TestEntry testEn = test.sub(r.testId);
        Bool isLd = testEn.memInst.mem_func == Ld;
        let data = testEn.stData;
        let paddr = testEn.paddr;
        function Tuple2#(ByteEn, Data) getShiftedBEData(Addr addr, ByteEn be, Data d);
            Bit#(TLog#(NumBytes)) byteOffset = truncate(addr);
            return tuple2(unpack(pack(be) << byteOffset), d << {byteOffset, 3'b0});
        endfunction
        let {shiftBE, shiftData} = getShiftedBEData(paddr, testEn.memInst.byteEn, data);
        // print
        $display("[doTlbResp] %t: ", $time, fshow(r), "; ", fshow(testEn));
        // upate LSQ, may set ReEx, may set Killed
        let res <- lsq.update(r.lsqTag, paddr, shiftBE, shiftData, isLd ? Valid (r.specTag) : Invalid);
        if(isLd && !res.waitWPResp) begin
            issueLd.wset(LSQIssueInfo {
                tag: r.lsqTag,
                paddr: paddr,
                shiftedBE: shiftBE
            });
        end
        // free spec tag except for Ld
        if(!isLd) begin
            globalSpecUpdate.correctSpec[tlbCorrectSpecPort].put(r.specTag);
        end
    endrule

    rule doKillLd;
        // get load to kill from LSQ
        LSQKillInfo en <- lsq.getKillLd;
        redirect(en.inst_tag, validValue(en.ldSpecTag));
        // stat
        killCnt <= killCnt + 1;
        // print
        $display("[doKillLd] %t: ", $time, fshow(en));
        // check specTag valid
        doAssert(isValid(en.ldSpecTag), "killed Ld must have spec tag");
    endrule

    function Action doIssueLd(LSQIssueInfo info, Bool fromIssueQ);
    action
        // search SB
        SBSearchRes sbRes = stb.search(info.paddr, info.shiftedBE);
        // search LSQ
        LSQIssueResult issRes <- lsq.issue(info.tag, info.paddr, info.shiftedBE, sbRes);
        $display("[doIssueLd] %t: fromIssueQ: ", $time, fshow(fromIssueQ), " ; ",
                 fshow(info), " ; ", fshow(sbRes), " ; ", fshow(issRes));
        // summarize
        if(issRes matches tagged Forward .forward) begin
            forwardQ.enq(tuple2(info.tag, forward.data));
        end
        else if(issRes == ToCache) begin
            ccm.procReq(zeroExtend(info.tag), MemReq {
                op: Ld,
                byteEn: replicate(False),
                addr: info.paddr,
                data: ?,
                amoInst: Invalid
            });
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

    rule doIssueLdFromUpdate(issueLd.wget matches tagged Valid .info);
        // issue the entry that just updates LSQ this cycle
        doIssueLd(info, False);
    endrule

    rule doRespLd;
        let {t, d} <- toGet(respLdQ).get;
        LSQRespLdResult res <- lsq.respLd(t, d);
        $display("[doRespLd] %t: ", $time, fshow(t), " ; ", fshow(d), " ; ", fshow(res));
    endrule

    let lsqDeqEn = lsq.first;

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
        globalSpecUpdate.correctSpec[lsqCorrectSpecPort].put(validValue(lsqDeqEn.ldSpecTag));

        // incr comptr & set wire to reset timeout
        comPtr <= comPtr + 1;
        commitInst.send;
        // log
        $fdisplay(respLog, "%d: ", comPtr, fshow(lsqDeqEn));
        // check result
        TestEntry testEn = test.sub(truncate(comPtr));
        $display("[doDeqLSQ_Ld] %t: comPtr %d; ", $time, comPtr,
            fshow(lsqDeqEn), "; ", fshow(testEn)
        );
        doAssert(comPtr == zeroExtend(lsqDeqEn.inst_tag), "comPtr != inst_tag");
        doAssert(testEn.resp == lsqDeqEn.shiftedData, "wrong Ld resp");
    endrule

    // we can deq St when (1) Done (2) no spec bit (3) can send to SB
    rule doDeqLSQ_St(
        lsqDeqEn.mem_inst.mem_func == St &&&
        lsqDeqEn.spec_bits == 0 &&& lsqDeqEn.computed &&&
        stb.getEnqIndex(lsqDeqEn.paddr) matches tagged Valid .sbIdx
    );
        lsq.deq;
        // send to SB
        stb.enq(sbIdx, lsqDeqEn.paddr, lsqDeqEn.shiftedBE, lsqDeqEn.shiftedData);

        // incr comptr & set wire to reset timeout
        comPtr <= comPtr + 1;
        commitInst.send;
        // log
        $fdisplay(respLog, "%d: ", comPtr, fshow(lsqDeqEn));
        // check result
        TestEntry testEn = test.sub(truncate(comPtr));
        $display("[doDeqLSQ_St] %t: comPtr %d; ", $time, comPtr,
            fshow(lsqDeqEn), "; ", fshow(testEn)
        );
        doAssert(comPtr == zeroExtend(lsqDeqEn.inst_tag), "comPtr != inst_tag");
        let {be, d} = scatterStore(testEn.paddr, testEn.memInst.byteEn, testEn.stData);
        doAssert(be == lsqDeqEn.shiftedBE && d == lsqDeqEn. shiftedData, "wrong BE or data");
    endrule

    // for Lr/Sc we first need to execution
    Reg#(Bool) waitLrScResp <- mkReg(False);

    Bool isDeqLrSc = (case(lsqDeqEn.mem_inst.mem_func)
        Lr, Sc: return True;
        default: return False;
    endcase);

    function MemOp func2op(MemFunc f);
        case(f)
            Ld: return Ld;
            Lr: return Lr;
            St: return St;
            Sc: return Sc;
            Amo: return Amo;
        endcase
    endfunction

    // issue Lr/Sc when
    // (1) not waiting for Lr/Sc resp
    // (2) addr/data is ready
    // (3) not pending on wrong path resp
    // (4) no spec bit
    // (5) SB does not match that addr
    rule doDeqLSQ_LrSc_issue(
        isDeqLrSc && !waitLrScResp && !lsqDeqEn.waitWPResp &&
        lsqDeqEn.computed && lsqDeqEn.spec_bits == 0 &&
        stb.noMatch(lsqDeqEn.paddr, lsqDeqEn.shiftedBE)
    );
        // send to mem
        waitLrScResp <= True;
        ccm.procReq(0, MemReq {
            op: func2op(lsqDeqEn.mem_inst.mem_func),
            byteEn: lsqDeqEn.shiftedBE,
            addr: lsqDeqEn.paddr,
            data: lsqDeqEn.shiftedData,
            amoInst: Invalid
        });
        // print
        $display("[doDeqLSQ_LrSc_issue] %t: ", $time, fshow(lsqDeqEn));
    endrule

    rule doDeqLSQ_LrSc_deq(isDeqLrSc && waitLrScResp);
        lsq.deq;
        waitLrScResp <= False;
        let data <- toGet(respLrScQ).get;
        Data resp = (case(lsqDeqEn.mem_inst.mem_func)
            Lr: return gatherLoad(lsqDeqEn.paddr, lsqDeqEn.mem_inst.byteEn, lsqDeqEn.mem_inst.unsignedLd, data); 
            Sc: return data;
            default: return ?;
        endcase);

        // incr comptr & set wire to reset timeout
        comPtr <= comPtr + 1;
        commitInst.send;
        // log
        $fdisplay(respLog, "%d: ", comPtr, fshow(lsqDeqEn), "; ", fshow(resp));
        // check result
        TestEntry testEn = test.sub(truncate(comPtr));
        $display("[doDeqLSQ_LrSc_deq] %t: comPtr %d; ", $time, comPtr,
            fshow(lsqDeqEn), "; ", fshow(resp), "; ", fshow(testEn)
        );
        doAssert(comPtr == zeroExtend(lsqDeqEn.inst_tag), "comPtr != inst_tag");
        doAssert(resp == testEn.resp, "wrong Lr/Sc resp");
    endrule

    rule doIssueSB;
        let {sbIdx, en} <- stb.issue;
        ccm.procReq(zeroExtend(sbIdx), MemReq {
            op: St,
            byteEn: ?,
            addr: {en.addr, 0},
            data: ?,
            amoInst: Invalid
        });
    endrule

    Wire#(Bool) stbEmpty_for_done <- mkBypassWire;
    Wire#(TestCnt) comPtr_for_done <- mkBypassWire;
    (* fire_when_enabled, no_implicit_conditions *)
    rule setForDone;
        stbEmpty_for_done <= stb.isEmpty;
        comPtr_for_done <= comPtr;
    endrule

    rule doneTest(state == Test && comPtr_for_done == fromInteger(valueof(TestNum)) && stbEmpty_for_done);
        state <= CheckMem;
        testCAddr <= 0;
        $fdisplay(stderr, "INFO: test done");
    endrule

    (* fire_when_enabled, no_implicit_conditions *)
    rule doTimeOut(state == Test);
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

    rule doCheckMem(state == CheckMem);
        // check mem value
        Addr a = {testCAddr, 0};
        let refLine = refMem.getCLine(a);
        let ccmLine = ccm.getCLine(a);
        $fdisplay(checkLog, "%x: %x, %x", a, refLine, ccmLine);
        doAssert(refLine == ccmLine, "wrong mem val");
        // change state
        testCAddr <= testCAddr + 1;
        if(testCAddr == fromInteger(valueof(CLineNum))) begin
            state <= Done;
            $fdisplay(stderr, "INFO: check mem done");
        end
    endrule

    rule doDone(state == Done);
        $fdisplay(stderr, "INFO: pass");
        $fdisplay(stderr, "stats: Ld %d, Lr %d, St %d, Sc %d, kill %d, dropResp %d",
            ldCnt, lrCnt, stCnt, scCnt, killCnt, dropRespCnt
        );
        $finish;
    endrule
endmodule
