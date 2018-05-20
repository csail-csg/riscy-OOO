import StmtFSM::*;
import Vector::*;
import Types::*;
import ProcTypes::*;
import TlbTypes::*;
import CacheUtils::*;
import HasSpecBits::*;
import DTlb::*;

typedef Data TestId;

typedef struct {
    Vpn vpn;
    TestId id;
} TestInst deriving(Bits, Eq, FShow);

function TlbReq getTlbReq(TestInst inst);
    PageOffset off = 0;
    return TlbReq {
        addr: zeroExtend({inst.vpn, off}), // vaddr page offset always 0
        write: False
    };
endfunction

typedef DTlb#(TestInst) Tlb;
(* synthesize *)
module mkTlb(Tlb);
    let m <- mkDTlb(getTlbReq);
    return m;
endmodule

// In our test, ppn always equals to {'1, vpn}
function Ppn getPpn(Vpn vpn);
    return {'1, vpn};
endfunction

function Addr getPAddr(Vpn vpn);
    PageOffset off = 0;
    return zeroExtend({getPpn(vpn), off});
endfunction

function TlbEntry getTlbEntry(Vpn vpn);
    return TlbEntry {
        vpn: vpn,
        ppn: getPpn(vpn),
        pteType: PTEType {
            dirty: True,
            accessed: True,
            global: True,
            user: True,
            executable: True,
            writable: True,
            readable: True
        },
        level: 0,
        asid: 0
    };
endfunction

function DTlbReq#(TestInst) getDTlbReq(Vpn vpn, TestId id, SpecBits sb);
    return DTlbReq {
        inst: TestInst {vpn: vpn, id: id},
        specBits: sb
    };
endfunction

(* synthesize *)
module mkTbUnit(Empty);
    let tlb <- mkTlb;

    Vector#(DTlbReqNum, Reg#(DTlbRqToP)) miss <- replicateM(mkRegU);

    Stmt test = (seq
        tlb.updateVMInfo(VMInfo {
            prv: prvU,
            asid: 0,
            sv39: True,
            exeReadable: True,
            userAccessibleByS: True,
            basePPN: 0
        });

        // simple req -> miss -> resp
        tlb.procReq(getDTlbReq(0, 0, 0));
        action
            tlb.toParent.rqToP.deq;
            let r = tlb.toParent.rqToP.first;
            doAssert(r.vpn == 0, "vpn match");
            tlb.toParent.ldTransRsFromP.enq(DTlbTransRsFromP {
                entry: Valid (getTlbEntry(r.vpn)),
                id: r.id
            });
        endaction
        action
            tlb.deqProcResp;
            let r = tlb.procResp;
            $display("tlb resp ", fshow(r));
            doAssert(r.inst.vpn == 0 && r.inst.id == 0, "inst match");
            doAssert(r.specBits == 0, "spec bits match");
            let {paddr, excep} = r.resp;
            doAssert(paddr == getPAddr(0), "ppn match"); 
            doAssert(excep == Invalid, "excep match");
        endaction

        // now tlb has vpn 0, try to hit on it
        tlb.procReq(getDTlbReq(0, 1, 0));
        action
            tlb.deqProcResp;
            let r = tlb.procResp;
            doAssert(r.inst.vpn == 0 && r.inst.id == 1, "inst match");
            doAssert(r.specBits == 0, "spec bits match");
            let {paddr, excep} = r.resp;
            doAssert(paddr == getPAddr(0), "ppn match"); 
            doAssert(excep == Invalid, "excep match");
        endaction

        // hit under one miss, the miss is subject to spec tag 0 and 3
        tlb.procReq(getDTlbReq(1, 2, zeroExtend(4'b1001)));
        action
            tlb.toParent.rqToP.deq;
            let r = tlb.toParent.rqToP.first;
            doAssert(r.vpn == 1, "vpn match");
            miss[0] <= r;
        endaction
        tlb.procReq(getDTlbReq(0, 3, 0));
        action
            tlb.deqProcResp;
            let r = tlb.procResp;
            doAssert(r.inst.vpn == 0 && r.inst.id == 3, "inst match");
            doAssert(r.specBits == 0, "spec bits match");
            let {paddr, excep} = r.resp;
            doAssert(paddr == getPAddr(0), "ppn match"); 
            doAssert(excep == Invalid, "excep match");
        endaction

        // clear spec tag 0
        tlb.specUpdate.correctSpeculation({'1, 1'b0});

        // add 2 more misses subject to spec tag 0, then hit under 3 misses
        tlb.procReq(getDTlbReq(2, 4, 1));
        tlb.procReq(getDTlbReq(3, 5, 1));
        action
            tlb.toParent.rqToP.deq;
            let r = tlb.toParent.rqToP.first;
            doAssert(r.vpn == 2, "vpn match");
            miss[1] <= r;
        endaction
        action
            tlb.toParent.rqToP.deq;
            let r = tlb.toParent.rqToP.first;
            doAssert(r.vpn == 3, "vpn match");
            miss[2] <= r;
        endaction
        tlb.procReq(getDTlbReq(0, 6, 0));
        action
            tlb.deqProcResp;
            let r = tlb.procResp;
            doAssert(r.inst.vpn == 0 && r.inst.id == 6, "inst match");
            doAssert(r.specBits == 0, "spec bits match");
            let {paddr, excep} = r.resp;
            doAssert(paddr == getPAddr(0), "ppn match"); 
            doAssert(excep == Invalid, "excep match");
        endaction

        // add 1 more miss subject to spec tag 1, so MSHR is full now
        tlb.procReq(getDTlbReq(4, 7, zeroExtend(2'b10)));
        action
            tlb.toParent.rqToP.deq;
            let r = tlb.toParent.rqToP.first;
            doAssert(r.vpn == 4, "vpn match");
            miss[3] <= r;
        endaction

        // resp to first 2 misses
        action
            DTlbRqToP r = miss[0];
            tlb.toParent.ldTransRsFromP.enq(DTlbTransRsFromP {
                entry: Valid (getTlbEntry(r.vpn)),
                id: r.id
            });
        endaction
        action
            DTlbRqToP r = miss[1];
            tlb.toParent.ldTransRsFromP.enq(DTlbTransRsFromP {
                entry: Valid (getTlbEntry(r.vpn)),
                id: r.id
            });
        endaction

        // Wait for the resp to be processed, and kill spec tag 0. This should
        // kill tests 4 and 5, but not tests 2 and 7.
        delay(100);
        tlb.specUpdate.incorrectSpeculation(False, 0);

        // get resp for test 2 on VPN 1
        action
            tlb.deqProcResp;
            let r = tlb.procResp;
            doAssert(r.inst.vpn == 1 && r.inst.id == 2, "inst match");
            doAssert(r.specBits == zeroExtend(4'b1000), "spec bits match");
            let {paddr, excep} = r.resp;
            doAssert(paddr == getPAddr(1), "ppn match"); 
            doAssert(excep == Invalid, "excep match");
        endaction

        // resp to last 2 misses
        action
            DTlbRqToP r = miss[2];
            tlb.toParent.ldTransRsFromP.enq(DTlbTransRsFromP {
                entry: Valid (getTlbEntry(r.vpn)),
                id: r.id
            });
        endaction
        action
            DTlbRqToP r = miss[3];
            tlb.toParent.ldTransRsFromP.enq(DTlbTransRsFromP {
                entry: Valid (getTlbEntry(r.vpn)),
                id: r.id
            });
        endaction

        // peek resp for test 7 on VPN 4
        action
            let r = tlb.procResp;
            doAssert(r.inst.vpn == 4 && r.inst.id == 7, "inst match");
            doAssert(r.specBits == zeroExtend(2'b10), "spec bits match");
            let {paddr, excep} = r.resp;
            doAssert(paddr == getPAddr(4), "ppn match"); 
            doAssert(excep == Invalid, "excep match");
        endaction
        
        // kill everything
        tlb.specUpdate.incorrectSpeculation(True, ?);

        // there should not be any pending req
        await(tlb.noPendingReq);

        $fdisplay(stderr, "PASS");
    endseq);

    mkAutoFSM(test);
endmodule
