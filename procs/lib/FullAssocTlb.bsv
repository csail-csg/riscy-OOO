
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

import Vector::*;
import Types::*;
import ProcTypes::*;
import TlbTypes::*;

typedef struct {
    Bool hit;
    indexT index; // hit index, for later update LRU info
    TlbEntry entry; // hit entry
} FullAssocTlbResp#(type indexT) deriving(Bits, Eq, FShow);

interface FullAssocTlb#(numeric type n);
    method Action flush;
    method FullAssocTlbResp#(Bit#(TLog#(n))) translate(Vpn vpn, Asid asid);
    method Action updateRep(Bit#(TLog#(n)) index); // update replacement info
    method Action addEntry(TlbEntry x);
endinterface

module mkFullAssocTlb(FullAssocTlb#(tlbSz)) provisos(
    Add#(1, a__, tlbSz),
    Alias#(tlbIdxT, Bit#(TLog#(tlbSz)))
);
    Vector#(tlbSz, Reg#(Bool))    validVec <- replicateM(mkReg(False));
    Vector#(tlbSz, Reg#(TlbEntry) entryVec <- replicateM(mkRegU);

    // bit-LRU replacement
    Reg#(Bit#(tlbSz)) lruBit <- mkReg(0);
    // when an entry is accesed, update LRU bits
    function Action updateLRU(tlbIdxT idx);
    action
        Bit#(tlbSz) val = lruBit;
        val[idx] = 1;
        if(val == maxBound) begin
            val = 0;
            val[idx] = 1;
        end
        lruBit <= val;
    endaction
    endfunction

    // function to match TLB entry
    method Action flush;
        writeVReg(validVec, replicate(False));
        lruBit <= 0; // also reset LRU bits
    endmethod

    method FullAssocTlbResp#(tlbIdxT) translate(Vpn vpn, Asid asid);
        // find the matching entry
        function Bool isMatch(Integer i);
            TlbEntry entry = entryVec[i];
            Bool asidMatch = entry.asid == asid || entry.pteType.global;
            Bool vpnMatch = getMaskedVpn(vpn, entry.level) == entry.vpn;
            return validVec[i] && asidMatch && vpnMatch;
        endfunction
        Vector#(tlbSz, Integer) idxVec = genVector;
        if(find(isMatch, idxVec) matches tagged Valid .idx) begin
            // hit a TLB entry, get its content
            return FullAssocTlbResp {
                hit: True,
                entry: entryVec[idx]
            };
        end
        else begin // miss
            return FullAssocTlbResp {
                hit: False,
                entry: ?
            };
        end
    endmethod

    method Action addEntry(TlbEntry x);
        // find a slot for this translation
        // Since TLB is read-only cache, we can silently evict
        tlbIdxT addIdx;
        if(findIndex( \== (False) , readVReg(validVec) ) matches tagged Valid .idx) begin
            // get empty slot
            addIdx = pack(idx);
        end
        else begin
            // find LRU slot (lruBit[i] = 0 means i is LRU slot)
            Vector#(tlbSz, Bool) isLRU = unpack(~lruBit);
            if(findIndex(id, isLRU) matches tagged Valid .idx) begin
                addIdx = pack(idx);
            end
            else begin
                addIdx = 0; // this is actually impossible
                doAssert(False, "must have at least 1 LRU slot");
            end
        end
        // update slot
        validVec[addIdx] <= True;
        entryVec[addIdx] <= x;
        // check ppn and vpn lower bits are 0 for super pages
        assert(x.ppn == getMaskedPpn(x.ppn, x.level), "ppn lower bits not 0");
        assert(x.vpn == getMaskedVpn(x.vpn, x.level), "vpn lower bits not 0");
        // update LRU bits
        updateLRU(addIdx);
    endmethod

    method Action updateRep(indexT index);
        updateLRU(index);
    endmethod
endmodule
