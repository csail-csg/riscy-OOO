
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
    // following fields are valid only when hit = True
    indexT index; // index of hitting entry
    TlbEntry entry; // hit entry
    // since page is in TLB, its access bit must have already been set
} FullAssocTlbResp#(type indexT) deriving(Bits, Eq, FShow);

interface FullAssocTlb#(numeric type n);
    method Action flush_translation(Vpn vpn, Asid asid, Bool all);
    method FullAssocTlbResp#(Bit#(TLog#(n))) translate(Vpn vpn, Asid asid);
    method Action update(Bit#(TLog#(n)) idx, Bool setDirty);
    method Action add_translation(TlbEntry x);
endinterface

module mkFullAssocTlb(FullAssocTlb#(tlbSz)) provisos(
    Add#(1, a__, tlbSz),
    Alias#(tlbIdxT, Bit#(TLog#(tlbSz)))
);
    Vector#(tlbSz, Reg#(Bool))     validVec    <- replicateM(mkReg(False));
    Vector#(tlbSz, Reg#(Vpn))      vpnVec      <- replicateM(mkRegU);
    Vector#(tlbSz, Reg#(Ppn))      ppnVec      <- replicateM(mkRegU);
    Vector#(tlbSz, Reg#(PTE_Type)) pagePermVec <- replicateM(mkRegU);
    Vector#(tlbSz, Reg#(PageSize)) pageSizeVec <- replicateM(mkRegU);
    Vector#(tlbSz, Reg#(Asid))     asidVec     <- replicateM(mkRegU);
    Vector#(tlbSz, Reg#(Bool))     dirtyVec    <- replicateM(mkRegU);

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
    function Bool isMatch(Vpn vpn, Asid asid, Integer i);
        Vpn masked_vpn = getMaskedVpn(vpn, pageSizeVec[i]);
        return validVec[i] && vpnVec[i] == masked_vpn && (asidVec[i] == asid || pagePermVec[i].global);
    endfunction

    method Action flush_translation(Vpn vpn, Asid asid, Bool all);
        // This could be used to select a line to flush:
        // But for now, flush the whole thing because the above solution would require
        // additional associative searches and linux doesn't use precise flushes yet
        doAssert(all, "only support flushing all");
        writeVReg(validVec, replicate(False));
        lruBit <= 0; // also reset LRU bits
    endmethod

    method FullAssocTlbResp#(tlbIdxT) translate(Vpn vpn, Asid asid);
        // find the matching entry
        Vector#(tlbSz, Bool) matchVec = map(isMatch(vpn, asid), genVector);
        if(findIndex(id, matchVec) matches tagged Valid .idx) begin
            // hit a TLB entry, get its content
            // sanity check: only 1 hit
            //doAssert(pack(matchVec) == (1 << idx), "at most 1 TLB hit");
            // return
            return FullAssocTlbResp {
                hit: True,
                index: pack(idx),
                entry: TlbEntry {
                    vpn: vpnVec[idx],
                    ppn: ppnVec[idx],
                    page_perm: pagePermVec[idx],
                    page_size: pageSizeVec[idx],
                    asid: asidVec[idx],
                    dirty: dirtyVec[idx]
                }
            };
        end
        else begin
            return FullAssocTlbResp {
                hit: False,
                index: ?,
                entry: ?
            };
        end
    endmethod

    method Action update(tlbIdxT idx, Bool setDirty);
        // entry is accessed, update LRU
        updateLRU(idx);
        // update dirty
        if(setDirty) begin
            dirtyVec[idx] <= True;
        end
    endmethod

    method Action add_translation(TlbEntry x);
        // find a slot for this translation
        // Since TLB is write-through, we can silently evict
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
        vpnVec[addIdx] <= x.vpn;
        ppnVec[addIdx] <= x.ppn;
        pagePermVec[addIdx] <= x.page_perm;
        pageSizeVec[addIdx] <= x.page_size;
        asidVec[addIdx] <= x.asid;
        dirtyVec[addIdx] <= x.dirty;
        // update LRU bits
        updateLRU(addIdx);
    endmethod
endmodule
