
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
import CacheUtils::*;
import Types::*;
import CCTypes::*;
import LLPipe::*;
import LLCRqMshr::*;
import LLBank::*;
import L1CoCache::*;
import LLCDmaConnect::*;

// Last-Level: 256KB per bank x 1 bank
typedef 8 LLWayNum;
typedef 0 LgLLBankNum;
typedef 9 LgLLSetNum;

typedef Bit#(LgLLBankNum) LLBankId;
typedef LgLLSetNum LLIndexSz;
typedef Bit#(LLIndexSz) LLIndex;
typedef GetTagSz#(LgLLBankNum, LgLLSetNum) LLTagSz;
typedef Bit#(LLTagSz) LLTag;
typedef Bit#(TLog#(LLWayNum)) LLWay;

typedef 8 LLCRqNum;
typedef Bit#(TLog#(LLCRqNum)) LLCRqMshrIdx;

// all L1$ are children
typedef L1Num LLChildNum;
typedef Bit#(TLog#(LLChildNum)) LLChild;
typedef L1Way LLCRqId;

(* synthesize *)
module mkLastLvCRqMshr(
    LLCRqMshr#(LLChildNum, LLCRqNum, LLWay, LLTag, cRqT)
) provisos(
    Alias#(cRqT, LLRq#(LLCRqId, LLCDmaReqId, LLChild))
);
    function Addr getAddr(cRqT r) = r.addr;
    let m <- mkLLCRqMshr(getAddr);
    return m;
endmodule

(* synthesize *)
module mkLLPipeline(
    LLPipe#(LgLLBankNum, LLChildNum, LLWayNum, LLIndex, LLTag, LLCRqMshrIdx)
);
    let m <- mkLLPipe;
    return m;
endmodule

typedef LLBank#(LgLLBankNum, LLChildNum, LLWayNum, LLIndexSz, LLTagSz, LLCRqNum, LLCRqId, LLCDmaReqId) LLCache;
typedef MemFifoClient#(LdMemRqId#(LLCRqMshrIdx), void) LLCMemFifoClient;
typedef LLCRqStuck#(LLChildNum, LLCRqId, LLCDmaReqId) LLCStuck;

(* synthesize *)
module mkLLCache(LLCache);
`ifdef DEBUG_DMA
    staticAssert(False, "DEBUG_DMA should not be defined");
`endif
    let m <- mkLLBank(mkLastLvCRqMshr, mkLLPipeline);
    return m;
endmodule
