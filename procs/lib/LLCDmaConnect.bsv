
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

import FShow::*;
import GetPut::*;
import Vector::*;
import BuildVector::*;
import FIFO::*;
import Assert::*;

import Types::*;
import ProcTypes::*;
import CacheUtils::*;
import CCTypes::*;
import L2Tlb::*;
import HostDmaLLC::*;
import CrossBar::*;

typedef struct {
    CoreId core;
    TlbMemReqId id;
    LineDataOffset dataSel;
} TlbDmaReqId deriving(Bits, Eq, FShow);

typedef union tagged {
    HostMemReqId Host;
    TlbDmaReqId Tlb;
} LLCDmaReqId deriving(Bits, Eq, FShow);

module mkLLCDmaConnect#(
    DmaServer#(LLCDmaReqId) llc,
    HostMemClient host,
    Vector#(CoreNum, TlbMemClient) tlb
)(Empty) provisos (
    Alias#(dmaRqT, DmaRq#(LLCDmaReqId))
);
    Bool verbose = True;

    // helper functions for cross bar
    function XBarDstInfo#(Bit#(0), Tuple2#(CoreId, TlbMemReq)) getTlbDst(CoreId core, TlbMemReq r);
        return XBarDstInfo {idx: 0, data: tuple2(core, r)};
    endfunction
    function Get#(TlbMemReq) tlbReqGet(TlbMemClient cli) = toGet(cli.memReq);

    // cross bar for Tlb
    FIFO#(Tuple2#(CoreId, TlbMemReq)) tlbQ <- mkFIFO;
    mkXBar(getTlbDst, map(tlbReqGet, tlb), vec(toPut(tlbQ)));

    // TLB req is for a whole data
    function dmaRqT getTlbDmaReq(CoreId c, TlbMemReq r);
        LineDataOffset dataSel = getLineDataOffset(r.addr);
        Line line = replicate(?);
        line[dataSel] = r.data;
        Vector#(LineSzData, Bit#(DataSzBytes)) be = replicate(0);
        if(r.write) begin
            be[dataSel] = maxBound;
        end
        let id = TlbDmaReqId {
            core: c,
            id: r.id,
            dataSel: dataSel
        };
        return DmaRq {
            addr: r.addr,
            byteEn: unpack(pack(be)),
            data: line,
            id: Tlb (id)
        };
    endfunction

    // send req to LLC
    rule sendHostReqToLLC;
        host.memReq.deq;
        let r = host.memReq.first;
        dmaRqT req =  DmaRq {
            addr: r.addr,
            byteEn: r.byteEn,
            data: r.data,
            id: Host (r.id)
        };
        llc.memReq.enq(req);
        if(verbose) begin
            $display("  [LLCDmaConnnect sendHostReqToLLC] ", fshow(r), " ; ", fshow(req));
        end
    endrule

    (* descending_urgency = "sendHostReqToLLC, sendTlbReqToLLC" *)
    rule sendTlbReqToLLC;
        let {c, r} <- toGet(tlbQ).get;
        let req = getTlbDmaReq(c, r);
        llc.memReq.enq(req);
        if(verbose) begin
            $display("  [LLCDmaConnnect sendTlbReqToLLC] ", fshow(r), " ; ", fshow(req));
        end
    endrule

    // send Ld resp from LLC
    rule sendLdRespToHost(llc.respLd.first.id matches tagged Host .id);
        llc.respLd.deq;
        let resp = llc.respLd.first;
        HostLdResp ld = DmaRs {
            data: resp.data,
            id: id
        };
        host.respLd.enq(ld);
        if(verbose) begin
            $display("  [LLCDmaConnect sendLdRespToHost] ", fshow(resp), " ; ", fshow(ld));
        end
    endrule

    rule sendLdRespToTlb(llc.respLd.first.id matches tagged Tlb .id);
        llc.respLd.deq;
        let resp = llc.respLd.first;
        let ld = TlbLdResp {
            data: resp.data[id.dataSel],
            id: id.id
        };
        tlb[id.core].respLd.enq(ld);
        if(verbose) begin
            $display("  [LLCDmaConnect sendLdRespToTlb] ", fshow(resp), " ; ", fshow(ld));
        end
    endrule

    // send St resp from LLC
    rule sendStRespToHost(llc.respSt.first matches tagged Host .id);
        llc.respSt.deq;
        host.respSt.enq(id);
        if(verbose) begin
            $display("  [LLCDmaConnect sendStRespToHost] ", fshow(llc.respSt.first));
        end
    endrule

    rule sendStRespToTlb(llc.respSt.first matches tagged Tlb .id);
        llc.respSt.deq;
        tlb[id.core].respSt.enq(id.id);
        if(verbose) begin
            $display("  [LLCDmaConnect sendStRespToTlb] ", fshow(llc.respSt.first));
        end
    endrule
endmodule
