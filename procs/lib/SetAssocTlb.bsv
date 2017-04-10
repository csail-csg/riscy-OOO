import Vector::*;
import Ehr::*;
import Types::*;
import ProcTypes::*;
import TlbTypes::*;
import RWBramCore::*;

// set assoc tlb only for 4KB pages
// indexed by VPN

typedef struct {
    Bool hit;
    wayT way; // hit/replace way
    TlbEntry entry; // hit entry, garbage for replacement
} SetAssocTlbResp#(type wayT) deriving(Bits, Eq, FShow);

typedef struct {
    Vpn vpn;
} SetAssocTlbReq deriving(Bits, Eq, FShow);

typedef struct {
    Bool valid;
    TlbEntry entry;
} SetAssocTlbEntry deriving(Bits, Eq, FShow);

interface SetAssocTlb#(
    numeric type wayNum,
    numeric type lgSetNum,
    type repInfoT // info for replacement, e.g. LRU
);
    method Action flush;
    method Bool flush_done;

    method Action req(SetAssocTlbReq r);
    method SetAssocTlbResp#(Bit#(TLog#(wayNum))) resp;
    // deq resp from pipeline and may update a way
    method Action deqUpdate(Bool update, Bit#(TLog#(wayNum)) way, SetAssocTlbEntry entry);
endinterface

typedef enum {Flush, Ready} SetAssocTlbState deriving(Bits, Eq, FShow);

module mkSetAssocTlb#(
    repInfoT repInfoInitVal,
    function wayT getRepWay(repInfoT info, Vector#(wayNum, Bool) invalid),
    function repInfoT updateRepInfo(repInfoT info, wayT way)
)(
    SetAssocTlb#(wayNum, lgSetNum, repInfoT)
) provisos(
    Alias#(wayT, Bit#(TLog#(wayNum))),
    Alias#(indexT, Bit#(lgSetNum)),
    Alias#(respT, SetAssocTlbResp#(wayT)),
    Bits#(repInfoT, a__),
    Add#(lgSetNum, b__, VpnSz)
);
    // ram for tlb entry
    Vector#(wayNum, RWBramCore#(indexT, SetAssocTlbEntry)) tlbRam <- replicateM(mkRWBramCore);
    // ram for replace info
    RWBramCore#(indexT, repInfoT) repRam <- mkRWBramCore;
    // pending
    Ehr#(2, Maybe#(SetAssocTlbReq)) pendReq <- mkEhr(Invalid);
    // init & flush index
    Reg#(indexT) flushIdx <- mkReg(0);
    // overall state
    Reg#(SetAssocTlbState) state <- mkReg(Flush);

    function indexT getIndex(Vpn vpn) = truncate(vpn);

    // we don't accept req when there is an old req to the same index
    Wire#(Maybe#(indexT)) pendIndex <- mkBypassWire;
    (* fire_when_enabled, no_implicit_conditions *)
    rule setPendIndex;
        pendIndex <= pendReq[0] matches tagged Valid .r ? Valid (getIndex(r.vpn)) : Invalid;
    endrule

    rule doFlush(state == Flush);
        // since TLB is write-through, we can discard everything
        for(Integer i = 0; i < valueof(wayNum); i = i+1) begin
            tlbRam[i].wrReq(flushIdx, SetAssocTlbEntry {valid: False, entry: ?});
        end
        repRam.wrReq(flushIdx, repInfoInitVal);
        // update states
        flushIdx <= flushIdx + 1;
        if(flushIdx == maxBound) begin
            // flush is done
            state <= Ready;
        end
    endrule

    // start flush when no pending req
    method Action flush if(state == Ready && !isValid(pendReq[0]));
        state <= Flush;
        flushIdx <= 0;
    endmethod

    method Bool flush_done = state == Ready;

    // accept new req when
    // (1) in Ready
    // (2) not waiting for flush
    // (3) pipeline reg available
    method Action req(SetAssocTlbReq r) if(state == Ready && !isValid(pendReq[1]));
        // (4) new req does not match index of existing req (no read/write race in BRAM)
        when(pendIndex != Valid (getIndex(r.vpn)), noAction);
        // store req
        pendReq[1] <= Valid (r);
        // read ram
        indexT idx = getIndex(r.vpn);
        for(Integer i = 0; i < valueof(wayNum); i = i+1) begin
            tlbRam[i].rdReq(idx);
        end
        repRam.rdReq(idx);
    endmethod

    method respT resp if(state == Ready &&& pendReq[0] matches tagged Valid .rq);
        // get all the tlb ram resp & LRU
        Vector#(wayNum, SetAssocTlbEntry) entries;
        for(Integer i = 0; i < valueof(wayNum); i = i+1) begin
            entries[i] = tlbRam[i].rdResp;
        end
        repInfoT repInfo = repRam.rdResp;
        // do VPN match (only 4KB page)
        function Bool vpnMatch(wayT i);
            return entries[i].valid && entries[i].entry.vpn == rq.vpn;
        endfunction
        Vector#(wayNum, wayT) wayVec = genWith(fromInteger);
        if(find(vpnMatch, wayVec) matches tagged Valid .w) begin
            // hit
            return SetAssocTlbResp {
                hit: True,
                way: w,
                entry: entries[w].entry
            };
        end
        else begin
            // miss: find way to replace
            Vector#(wayNum, Bool) invalid; // invalid ways
            for(Integer i = 0; i < valueof(wayNum); i = i+1) begin
                invalid[i] = !entries[i].valid;
            end
            wayT repWay = getRepWay(repInfo, invalid);
            return SetAssocTlbResp {
                hit: False,
                way: repWay,
                entry: ?
            };
        end
    endmethod

    // deq resp from pipeline and update a way and LRU
    method Action deqUpdate(Bool update, wayT way, SetAssocTlbEntry entry) if(
        state == Ready &&& pendReq[0] matches tagged Valid .rq
    );
        // deq pipeline reg
        pendReq[0] <= Invalid;
        // deq ram read resp
        for(Integer i = 0; i < valueof(wayNum); i = i+1) begin
            tlbRam[i].deqRdResp;
        end
        repRam.deqRdResp;
        // update ram
        let idx = getIndex(rq.vpn);
        let repInfo = repRam.rdResp;
        if(update) begin
            doAssert(entry.valid, "new entry must be valid");
            tlbRam[way].wrReq(idx, entry);
            repRam.wrReq(idx, updateRepInfo(repInfo, way));
        end
    endmethod
endmodule
