
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

import Assert::*;
import FIFO::*;
import BRAMFIFO::*;
import GetPut::*;
import FShow::*;

import Types::*;
import CacheUtils::*;
import CCTypes::*;
import DDR3Wrapper::*;
import DDR3Common::*;
import LLCache::*;

module mkDDR3LLC#(
    DDR3User ddr3,
    MemFifoClient#(idT, childT) llc,
    Integer maxReadNum, Bool useBramFifo
)(Empty) provisos(
    Alias#(idT, LdMemRqId#(LLCRqMshrIdx)), // LLC Ld mem req ID
    Alias#(childT, void), // single LLC as child of DDR3
    Add#(SizeOf#(Line), 0, DDR3UserDataSz) // make sure Line sz = DDR3 data sz
);
    Bool verbose = True;

    function DDR3UserAddr getDDR3AddrFromLLC(Addr a);
        return truncate(a >> valueof(TLog#(TDiv#(DDR3UserDataSz, 8))));
    endfunction

    FIFO#(Tuple2#(idT, childT)) pendRdQ = ?;
    if(useBramFifo) begin
        pendRdQ <- mkSizedBRAMFIFO(maxReadNum);
    end
    else begin
        pendRdQ <- mkSizedFIFO(maxReadNum);
    end

    rule doReq;
        llc.toM.deq;
        case(llc.toM.first) matches
            tagged Ld .ld: begin
                let ddr3Req = DDR3UserReq {
                    addr: getDDR3AddrFromLLC(ld.addr),
                    data: ?,
                    wrBE: 0
                };
                ddr3.req(ddr3Req);
                pendRdQ.enq(tuple2(ld.id, ld.child));
                if(verbose) begin
                    $display("  [DDR3LLC doReq] Ld: ", fshow(ld), " ; ", fshow(ddr3Req));
                end
            end
            tagged Wb .wb: begin
                let ddr3Req = DDR3UserReq {
                    addr: getDDR3AddrFromLLC(wb.addr),
                    data: pack(wb.data),
                    wrBE: pack(wb.byteEn)
                };
                ddr3.req(ddr3Req);
                if(verbose) begin
                    $display("  [DDR3LLC doReq] St: ", fshow(wb), " ; ", fshow(ddr3Req));
                end
                doAssert(ddr3Req.wrBE != 0, "St req cannot have all 0 BE");
            end
            default: begin
                doAssert(False, "unknown LLC req");
            end
        endcase
    endrule

    rule doLdResp;
        let data <- ddr3.rdResp;
        let {id, child} <- toGet(pendRdQ).get;
        MemRsMsg#(idT, childT) resp = MemRsMsg {
            data: unpack(data),
            child: child,
            id: id
        };
        llc.rsFromM.enq(resp);
        if(verbose) begin
            $display("  [DDR3LLC doLdResp] ", fshow(resp));
        end
    endrule
endmodule
