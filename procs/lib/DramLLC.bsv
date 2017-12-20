
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
import DramCommon::*;
import LLCache::*;

module mkDramLLC#(
    DramUser#(dramReadNum, dramWriteNum, dramSimDelay, dramErrT) dram,
    MemFifoClient#(idT, childT) llc,
    Integer maxReadNum, Bool useBramFifo
)(Empty) provisos(
    Alias#(idT, LdMemRqId#(LLCRqMshrIdx)), // LLC Ld mem req ID
    Alias#(childT, void), // single LLC as child of DRAM
    Add#(SizeOf#(Line), 0, DramUserDataSz) // make sure Line sz = Dram data sz
);
    Bool verbose = True;

    function DramUserAddr getDramAddrFromLLC(Addr a);
        return truncate(a >> valueof(TLog#(DramUserBESz)));
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
                let dramReq = DramUserReq {
                    addr: getDramAddrFromLLC(ld.addr),
                    data: ?,
                    wrBE: 0
                };
                dram.req(dramReq);
                pendRdQ.enq(tuple2(ld.id, ld.child));
                if(verbose) begin
                    $display("  [DramLLC doReq] Ld: ", fshow(ld), " ; ", fshow(dramReq));
                end
            end
            tagged Wb .wb: begin
                let dramReq = DramUserReq {
                    addr: getDramAddrFromLLC(wb.addr),
                    data: pack(wb.data),
                    wrBE: pack(wb.byteEn)
                };
                dram.req(dramReq);
                if(verbose) begin
                    $display("  [DramLLC doReq] St: ", fshow(wb), " ; ", fshow(dramReq));
                end
                doAssert(dramReq.wrBE != 0, "St req cannot have all 0 BE");
            end
            default: begin
                doAssert(False, "unknown LLC req");
            end
        endcase
    endrule

    rule doLdResp;
        let data <- dram.rdResp;
        let {id, child} <- toGet(pendRdQ).get;
        MemRsMsg#(idT, childT) resp = MemRsMsg {
            data: unpack(data),
            child: child,
            id: id
        };
        llc.rsFromM.enq(resp);
        if(verbose) begin
            $display("  [DramLLC doLdResp] ", fshow(resp));
        end
    endrule
endmodule
