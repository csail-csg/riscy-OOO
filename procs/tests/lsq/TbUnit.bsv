// simple tests for LSQ

import StmtFSM::*;
import Vector::*;

import Types::*;
import MemoryTypes::*;
import ProcTypes::*;
import CCTypes::*;
import SplitLSQ::*;
import StoreBuffer::*;

(* synthesize *)
module mkTbLdIssueLate(FSM);
    SplitLSQ lsq <- mkSplitLSQ;

    // Ld req
    InstTag instTag = InstTag {
        way: 0,
        ptr: 0,
        t: 0
    };
    MemInst memInst = MemInst {
        mem_func: Ld,
        amo_func: ?,
        unsignedLd: True,
        byteEn: replicate(True),
        aq: False,
        rl: False
    };
    PhyDst dst = PhyDst {
        indx: 2,
        isFpuReg: False
    };
    SpecTag specTag = 2;
    SpecBits specBits = 1 << specTag; // depend on itself
    Addr paddr = 128;
    Bool isMMIO = False;
    ByteEn shiftedBE = replicate(True);
    Data respData = 42;

    Reg#(LdQTag) ldTag <- mkRegU;

    Stmt test = (seq
        $display("[TbLdIssueLate] start");
        action
            Maybe#(LdStQTag) tag = lsq.enqTag(Ld);
            $display("[TbLdIssueLate] enq tag = ", fshow(tag));
            doAssert(isValid(tag), "lsq should have entry");
            // record tag
            if(validValue(tag) matches tagged Ld .t) begin
                ldTag <= t;
            end
            else begin
                doAssert(False, "should have Ld tag");
            end
            lsq.enqLd(instTag, memInst, Valid (dst), specBits);
        endaction
        action
            let res <- lsq.updateAddr(Ld (ldTag), paddr, isMMIO,
                                      shiftedBE, Valid (specTag));
            $display("[TbLdIssueLate] update result = ", fshow(res));
            doAssert(!res.waitWPResp, "wrong update result");
        endaction
        action
            LSQIssueLdInfo info <- lsq.getIssueLd;
            $display("[TbLdIssueLate] issue info = ", fshow(info));
            let sbRes = SBSearchRes {
                matchIdx: Invalid,
                forwardData: Invalid
            };
            LSQIssueLdResult res <- lsq.issueLd(ldTag, paddr,
                                                shiftedBE, sbRes);
            $display("[TbLdIssueLate] issue result = ", fshow(res));
            doAssert(res == ToCache, "wrong issue result");
        endaction
        action
            LSQRespLdResult res <- lsq.respLd(ldTag, respData);
            $display("[TbLdIssueLate] resp result = ", fshow(res));
            doAssert(!res.wrongPath &&
                     res.data == respData &&
                     res.dst == Valid (dst),
                     "wrong resp result");
        endaction
        action
            lsq.deqLd;
            LdQDeqEntry deqEn = lsq.firstLd;
            $display("[TbLdIssueLate] deq entry = ", fshow(deqEn));
            doAssert(deqEn.instTag == instTag &&
                     deqEn.memFunc == Ld &&
                     deqEn.rel == memInst.rl &&
                     deqEn.dst == Valid (dst) &&
                     deqEn.paddr == paddr &&
                     deqEn.isMMIO == isMMIO &&
                     deqEn.shiftedBE == shiftedBE &&
                     deqEn.specTag == Valid (specTag) &&
                     deqEn.specBits == specBits &&
                     deqEn.waitWPResp == False,
                     "wrong deq entry");
        endaction
        $display("[TbLdIssueLate] PASS");
    endseq);

    FSM fsm <- mkFSM(test);
    return fsm;
endmodule

(* synthesize *)
module mkTbUnit(Empty);
    let tbLdIssueLate <- mkTbLdIssueLate;

    Stmt test = (seq
        $fdisplay(stderr, "Test load issue late");
        tbLdIssueLate.start;
        tbLdIssueLate.waitTillDone;
        $fdisplay(stderr, "PASS");

        $fdisplay(stderr, "ALL PASS");
    endseq);
    mkAutoFSM(test);
endmodule
