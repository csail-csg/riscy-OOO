`include "ProcConfig.bsv"
import StmtFSM::*;
import Assert::*;
import Types::*;
import ProcTypes::*;
import TlbTypes::*;
import FullAssocTlb::*;

typedef `TLB_SIZE TlbSize;
typedef Bit#(TLog#(TlbSize)) TlbIdx;

typedef FullAssocTlb#(TlbSize) Tlb;
(* synthesize *)
module mkTlb(Tlb);
    let m <- mkFullAssocTlb(False); // no random
    return m;
endmodule

// In our test, ppn always equals to vpn. We use 2*TlbSize VPNs, i.e., 0 ~
// 2*TlbSize-1.

function TlbEntry getTlbEntry(Vpn vpn);
    return TlbEntry {
        vpn: vpn,
        ppn: zeroExtend(vpn),
        pteType: unpack(0),
        level: 0,
        asid: 0
    };
endfunction

(* synthesize *)
module mkTbUnit(Empty);
    Tlb tlb <- mkTlb;

    Integer tlbSize = valueof(TlbSize);
    Integer halfTlbSize = tlbSize / 2;
    staticAssert(tlbSize % 2 == 0, "tlb size must be even");

    Reg#(Vpn) testVpn <- mkReg(0);

    Stmt test = (seq
        //
        // test not adding repeated entry
        //
        tlb.flush;

        // fill in VPNs: 0 ~ tlbSize-1
        testVpn <= 0;
        while(testVpn < fromInteger(tlbSize))
        action
            tlb.addEntry(getTlbEntry(testVpn));
            testVpn <= testVpn + 1;
        endaction

        // check all VPNs hit
        testVpn <= 0;
        while(testVpn < fromInteger(tlbSize))
        action
            let res = tlb.translate(testVpn, 0);
            doAssert(res.hit, "must hit");
            doAssert(res.index == truncate(testVpn), "hit index = vpn");
            doAssert(res.entry == getTlbEntry(testVpn), "entry must match");
            tlb.updateRepByHit(res.index);
            testVpn <= testVpn + 1;
        endaction

        // fill in existing VPN
        testVpn <= 0;
        while(testVpn < fromInteger(tlbSize))
        action
            tlb.addEntry(getTlbEntry(fromInteger(halfTlbSize)));
            testVpn <= testVpn + 1;
        endaction

        // check all VPNs hit (i.e., nothing gets replaced)
        testVpn <= 0;
        while(testVpn < fromInteger(tlbSize))
        action
            let res = tlb.translate(testVpn, 0);
            doAssert(res.hit, "must hit");
            doAssert(res.index == truncate(testVpn), "hit index = vpn");
            doAssert(res.entry == getTlbEntry(testVpn), "entry must match");
            tlb.updateRepByHit(res.index);
            testVpn <= testVpn + 1;
        endaction


        //
        // test bit LRU replacement
        //

        tlb.flush;
        
        // fill in VPNs: 0 ~ tlbSize/2-1. They should be added to tlb idx 0 ~
        // tlbSize/2-1
        testVpn <= 0;
        while(testVpn < fromInteger(halfTlbSize))
        action
            tlb.addEntry(getTlbEntry(testVpn));
            testVpn <= testVpn + 1;
        endaction

        // check all VPNs hit or not
        testVpn <= 0;
        while(testVpn < fromInteger(halfTlbSize))
        action
            let res = tlb.translate(testVpn, 0);
            doAssert(res.hit, "must hit");
            doAssert(res.index == truncate(testVpn), "hit index = vpn");
            doAssert(res.entry == getTlbEntry(testVpn), "entry must match");
            tlb.updateRepByHit(res.index);
            testVpn <= testVpn + 1;
        endaction
        while(testVpn < fromInteger(tlbSize * 2))
        action
            let res = tlb.translate(testVpn, 0);
            doAssert(!res.hit, "cannot hit");
            testVpn <= testVpn + 1;
        endaction

        // fill in VPNs: tlbSize/2 ~ tlbSize-1. They should be added to tlb idx
        // tlbSize/2 ~ tlbSize-1.
        testVpn <= fromInteger(halfTlbSize);
        while(testVpn < fromInteger(tlbSize))
        action
            tlb.addEntry(getTlbEntry(testVpn));
            testVpn <= testVpn + 1;
        endaction

        // check all VPNs hit or not
        testVpn <= 0;
        while(testVpn < fromInteger(tlbSize))
        action
            let res = tlb.translate(testVpn, 0);
            doAssert(res.hit, "must hit");
            doAssert(res.index == truncate(testVpn), "hit index = vpn");
            doAssert(res.entry == getTlbEntry(testVpn), "entry must match");
            tlb.updateRepByHit(res.index);
            testVpn <= testVpn + 1;
        endaction
        while(testVpn < fromInteger(tlbSize * 2))
        action
            let res = tlb.translate(testVpn, 0);
            doAssert(!res.hit, "cannot hit");
            testVpn <= testVpn + 1;
        endaction

        // now all LRU bits should be 0 except last two, we hit VPNs 0 ~
        // tlbSize/2-1, so that they are MRU entries
        testVpn <= 0;
        while(testVpn < fromInteger(halfTlbSize))
        action
            let res = tlb.translate(testVpn, 0);
            doAssert(res.hit, "must hit");
            doAssert(res.index == truncate(testVpn), "hit index = vpn");
            doAssert(res.entry == getTlbEntry(testVpn), "entry must match");
            tlb.updateRepByHit(res.index);
            testVpn <= testVpn + 1;
        endaction

        // add entries VPNs tlbSize ~ tlbSize/2*3-1, they should be at idx
        // tlbSize/2 ~ tlbSize-3, 0, 1.
        testVpn <= fromInteger(tlbSize);
        while(testVpn < fromInteger(halfTlbSize + tlbSize))
        action
            tlb.addEntry(getTlbEntry(testVpn));
            testVpn <= testVpn + 1;
        endaction

        // check all VPNs hit or not
        testVpn <= 0;
        while(testVpn < 2)
        action
            let res = tlb.translate(testVpn, 0);
            $display("translate %d, ", testVpn, fshow(res));
            doAssert(!res.hit, "cannot hit");
            testVpn <= testVpn + 1;
        endaction
        while(testVpn < fromInteger(halfTlbSize))
        action
            let res = tlb.translate(testVpn, 0);
            $display("translate %d, ", testVpn, fshow(res));
            doAssert(res.hit, "must hit");
            doAssert(res.index == truncate(testVpn), "hit index = vpn");
            doAssert(res.entry == getTlbEntry(testVpn), "entry must match");
            tlb.updateRepByHit(res.index);
            testVpn <= testVpn + 1;
        endaction
        while(testVpn < fromInteger(tlbSize - 2))
        action
            let res = tlb.translate(testVpn, 0);
            $display("translate %d, ", testVpn, fshow(res));
            doAssert(!res.hit, "cannot hit");
            testVpn <= testVpn + 1;
        endaction
        while(testVpn < fromInteger(tlbSize))
        action
            let res = tlb.translate(testVpn, 0);
            $display("translate %d, ", testVpn, fshow(res));
            doAssert(res.hit, "must hit");
            doAssert(res.index == truncate(testVpn), "hit index = vpn");
            doAssert(res.entry == getTlbEntry(testVpn), "entry must match");
            tlb.updateRepByHit(res.index);
            testVpn <= testVpn + 1;
        endaction
        while(testVpn < fromInteger(halfTlbSize + tlbSize - 2))
        action
            let res = tlb.translate(testVpn, 0);
            $display("translate %d, ", testVpn, fshow(res));
            doAssert(res.hit, "must hit");
            doAssert(res.index == truncate(testVpn - fromInteger(halfTlbSize)), "hit index = vpn - tlbSize/2");
            doAssert(res.entry == getTlbEntry(testVpn), "entry must match");
            tlb.updateRepByHit(res.index);
            testVpn <= testVpn + 1;
        endaction
        action
            let res = tlb.translate(testVpn, 0);
            $display("translate %d, ", testVpn, fshow(res));
            doAssert(res.hit, "must hit");
            doAssert(res.index == 0, "hit index = 0");
            doAssert(res.entry == getTlbEntry(testVpn), "entry must match");
            tlb.updateRepByHit(res.index);
            testVpn <= testVpn + 1;
        endaction
        action
            let res = tlb.translate(testVpn, 0);
            $display("translate %d, ", testVpn, fshow(res));
            doAssert(res.hit, "must hit");
            doAssert(res.index == 1, "hit index = 1");
            doAssert(res.entry == getTlbEntry(testVpn), "entry must match");
            tlb.updateRepByHit(res.index);
            testVpn <= testVpn + 1;
        endaction
        while(testVpn < fromInteger(tlbSize * 2))
        action
            let res = tlb.translate(testVpn, 0);
            $display("translate %d, ", testVpn, fshow(res));
            doAssert(!res.hit, "cannot hit");
            testVpn <= testVpn + 1;
        endaction

        $fdisplay(stderr, "PASS");
    endseq);

    mkAutoFSM(test);
endmodule
