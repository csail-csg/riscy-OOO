
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

`include "ProcConfig.bsv"

import GetPut::*;
import Assert::*;
import CacheUtils::*;
import Fifo::*;
import Types::*;
import ProcTypes::*;
import CCTypes::*;
import LLPipe::*;
import LLCRqMshr::*;
import LLBank::*;
import L1CoCache::*;
import LLCDmaConnect::*;
import Performance::*;

// Last-Level: 1MB per core, no more than 2MB
typedef TMin#(15, TAdd#(14, TLog#(CoreNum))) LgLLLineNum;
typedef `LOG_LLC_WAYS LgLLWayNum;
typedef TExp#(LgLLWayNum) LLWayNum;
typedef 0 LgLLBankNum;
typedef TSub#(LgLLLineNum, TAdd#(LgLLWayNum, LgLLBankNum)) LgLLSetNum;

typedef Bit#(LgLLBankNum) LLBankId;
typedef LgLLSetNum LLIndexSz;
typedef Bit#(LLIndexSz) LLIndex;
typedef GetTagSz#(LgLLBankNum, LgLLSetNum) LLTagSz;
typedef Bit#(LLTagSz) LLTag;
typedef Bit#(TLog#(LLWayNum)) LLWay;

typedef LLWayNum LLCRqNum;
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

typedef LLBank#(LgLLBankNum, LLChildNum, LLWayNum, LLIndexSz, LLTagSz, LLCRqNum, LLCRqId, LLCDmaReqId) LLBankWrapper;
//typedef MemFifoClient#(LdMemRqId#(LLCRqMshrIdx), void) LLCMemFifoClient;
typedef LLCRqStuck#(LLChildNum, LLCRqId, LLCDmaReqId) LLCStuck;

interface LLCache;
    interface ParentCacheToChild#(LLCRqId, LLChild) to_child;
    interface DmaServer#(LLCDmaReqId) dma;
    interface MemFifoClient#(LdMemRqId#(LLCRqMshrIdx), void) to_mem;
    // detect deadlock: only in use when macro CHECK_DEADLOCK is defined
    interface Get#(LLCStuck) cRqStuck;
    // performance
    interface Perf#(LLCPerfType) perf;
endinterface

(* synthesize *)
module mkLLCache(LLCache);
`ifdef DEBUG_DMA
    staticAssert(False, "DEBUG_DMA should not be defined");
`endif

    LLBankWrapper cache <- mkLLBank(mkLastLvCRqMshr, mkLLPipeline);

    // perf counters
    Fifo#(1, LLCPerfType) perfReqQ <- mkCFFifo;
`ifdef PERF_COUNT
    Fifo#(1, PerfResp#(LLCPerfType)) perfRespQ <- mkCFFifo;

    rule doPerf;
        let t <- toGet(perfReqQ).get;
        let d = cache.getPerfData(t);
        perfRespQ.enq(PerfResp {
            pType: t,
            data: d
        });
    endrule
`endif

    interface to_child = cache.to_child;
    interface dma = cache.dma;
    interface to_mem = cache.to_mem;
    interface cRqStuck = cache.cRqStuck;

    interface Perf perf;
        method Action setStatus(Bool stats);
            cache.setPerfStatus(stats);
        endmethod
        method Action req(LLCPerfType r);
            perfReqQ.enq(r);
        endmethod
        method ActionValue#(PerfResp#(LLCPerfType)) resp;
`ifdef PERF_COUNT
            perfRespQ.deq;
            return perfRespQ.first;
`else
            perfReqQ.deq;
            return PerfResp {
                pType: perfReqQ.first,
                data: 0
            };
`endif
        endmethod
        method Bool respValid;
`ifdef PERF_COUNT
            return perfRespQ.notEmpty;
`else
            return perfReqQ.notEmpty;
`endif
        endmethod
    endinterface
endmodule
