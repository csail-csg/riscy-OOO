
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
import LLCRqMshrSecureModel::*;
import LLBank::*;
import L1CoCache::*;
import LLCDmaConnect::*;
import Performance::*;

// Last-Level

// whether we model the effect of MSHR partition for security purpose
`ifdef SECURITY
`ifndef DISABLE_SECURE_LLC_MSHR
`define USE_LLC_MSHR_SECURE_MODEL
`endif
`endif

// whether we model the effect of the circular/fair arbiter at the entry point
// of LLC pipeline for security purpose
`ifdef SECURITY
`ifndef DISABLE_SECURE_LLC_ARBITER
`define USE_LLC_ARBITER_SECURE_MODEL
`endif
`endif

typedef `LOG_LLC_LINES LgLLLineNum;
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

`ifdef USE_LLC_MSHR_SECURE_MODEL
typedef TDiv#(DramMaxReqs, 2) LLCRqNum; // SECURITY: limit MSHR size <= DRAM bandwidth
`else
typedef LLWayNum LLCRqNum;
`endif
typedef Bit#(TLog#(LLCRqNum)) LLCRqMshrIdx;

// all L1$ are children
typedef L1Num LLChildNum;
typedef Bit#(TLog#(LLChildNum)) LLChild;
typedef L1Way LLCRqId;

`ifdef USE_LLC_MSHR_SECURE_MODEL
module mkLastLvCRqMshr(
    LLCRqMshr#(LLChildNum, LLCRqNum, LLWay, LLTag, cRqT)
) provisos(
    Alias#(cRqT, LLRq#(LLCRqId, LLCDmaReqId, LLChild))
);
    function Addr getAddr(cRqT r) = r.addr;
    LLCRqMshrSecureModel#(
        `SIM_LOG_LLC_MSHR_BANK_NUM, LLChildNum, LLCRqNum, LLWay, LLTag, cRqT
    ) m <- mkLLCRqMshrSecureModel(getAddr);
    return m.mshr;
endmodule

`else

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
`endif

`ifdef USE_LLC_ARBITER_SECURE_MODEL
typedef `SIM_LLC_ARBITER_NUM SimLLCArbNum;
(* synthesize *)
module mkLLPipeline(
    LLPipe#(LgLLBankNum, LLChildNum, LLWayNum, LLIndex, LLTag, LLCRqMshrIdx)
);
    // pipeline
    LLPipe#(LgLLBankNum, LLChildNum, LLWayNum, LLIndex, LLTag, LLCRqMshrIdx) m <- mkLLPipe;

    // round-robin reg: only allow entry to pipeline when turn == 0. This
    // models the effect of a circular/fair arbiter.
    Reg#(Bit#(TLog#(SimLLCArbNum))) turn <- mkReg(0);

    (* fire_when_enabled, no_implicit_conditions *)
    rule incrTurn;
        turn <= turn == fromInteger(valueof(SimLLCArbNum) - 1) ? 0 : turn + 1;
    endrule

    method Action send(LLPipeIn#(LLChild, LLWay, LLCRqMshrIdx) r) if(turn == 0);
        m.send(r);
    endmethod

    method notEmpty = m.notEmpty;
    method first = m.first;
    method unguard_first = m.unguard_first;
    method deqWrite = m.deqWrite;
endmodule

`else

(* synthesize *)
module mkLLPipeline(
    LLPipe#(LgLLBankNum, LLChildNum, LLWayNum, LLIndex, LLTag, LLCRqMshrIdx)
);
    let m <- mkLLPipe;
    return m;
endmodule
`endif

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

`ifdef SECURITY
// We rotate the addr in/out LLC to achieve set partition
// FIXME This is a hack: we simulate the performance of partitioning a large
// LLC using a smaller LLC with less partitions. So LgLLCPartitionNum should
// NOT be viewed as the number of DRAM regions.
`ifdef SIM_LOG_LLC_PARTITION_NUM
typedef `SIM_LOG_LLC_PARTITION_NUM LgLLCPartitionNum;
`else
typedef `LOG_DRAM_REGION_NUM LgLLCPartitionNum;
`endif
typedef `LOG_DRAM_REGION_SIZE LgDramRegionSz;
typedef TAdd#(TAdd#(LLIndexSz, LgLLBankNum), LgLineSzBytes) LLIndexBankOffsetSz;

function Addr secureRotateAddr(Addr addr) provisos(
    // region/partition id cannot be wider than index + bank id
    Add#(LgLLCPartitionNum, a__, TAdd#(LLIndexSz, LgLLBankNum))
);
    // low bits: index + bank id + line offset without the higher bits which
    // will be replaced by region/partition id
    Bit#(TSub#(LLIndexBankOffsetSz, LgLLCPartitionNum)) low = truncate(addr);
    // swap bits: higher bits of index + bank id to be swapped with region/partition
    // id
    Bit#(LgLLCPartitionNum) swap = truncate(addr >> (valueof(LLIndexBankOffsetSz) - valueof(LgLLCPartitionNum)));
    // middle bits between swap and region
    Bit#(TSub#(LgDramRegionSz, LLIndexBankOffsetSz)) mid = truncate(addr >> valueof(LLIndexBankOffsetSz));
    // region/partition id
    Bit#(LgLLCPartitionNum) region = truncate(addr >> valueof(LgDramRegionSz));
    // high bits beyond phy mem boundary
    Bit#(TSub#(AddrSz, TAdd#(LgLLCPartitionNum, LgDramRegionSz))) high = truncateLSB(addr);
    // exchange swap bits with region bits
    return {high, swap, mid, region, low};
endfunction
`endif

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

`ifdef SECURITY
`ifndef DISABLE_SECURE_LLC

`ifdef BSIM
    // print the LLC partition in simulation
    Reg#(Bool) showLLCPartition <- mkReg(True);
    rule doShowLLCPartition(showLLCPartition);
        $fdisplay(stderr, "[LLCache] log LLC partition = %d", valueof(LgLLCPartitionNum));
        showLLCPartition <= False;
    endrule
`endif

    // rotate addr to achieve LLC set partition
    interface ParentCacheToChild to_child;
        interface FifoEnq rsFromC;
            method notFull = cache.to_child.rsFromC.notFull;
            method Action enq(CRsMsg#(LLChild) x);
                let y = x;
                y.addr = secureRotateAddr(x.addr);
                cache.to_child.rsFromC.enq(y);
            endmethod
        endinterface
        interface FifoEnq rqFromC;
            method notFull = cache.to_child.rqFromC.notFull;
            method Action enq(CRqMsg#(LLCRqId, LLChild) x);
                let y = x;
                y.addr = secureRotateAddr(x.addr);
                cache.to_child.rqFromC.enq(y);
            endmethod
        endinterface
        interface FifoDeq toC;
            method notEmpty = cache.to_child.toC.notEmpty;
            method deq = cache.to_child.toC.deq;
            method PRqRsMsg#(LLCRqId, LLChild) first;
                case(cache.to_child.toC.first) matches
                    tagged PRq .x: begin
                        let y = x;
                        y.addr = secureRotateAddr(x.addr);
                        return PRq (y);
                    end
                    tagged PRs .x: begin
                        let y = x;
                        y.addr = secureRotateAddr(x.addr);
                        return PRs (y);
                    end
                    default: return ?;
                endcase
            endmethod
        endinterface
    endinterface

    interface DmaServer dma;
        interface FifoEnq memReq;
            method notFull = cache.dma.memReq.notFull;
            method Action enq(DmaRq#(LLCDmaReqId) x);
                let y = x;
                y.addr = secureRotateAddr(x.addr);
                cache.dma.memReq.enq(y);
            endmethod
        endinterface
        interface respLd = cache.dma.respLd;
        interface respSt = cache.dma.respSt;
    endinterface

    interface MemFifoClient to_mem;
        interface FifoDeq toM;
            method notEmpty = cache.to_mem.toM.notEmpty;
            method deq = cache.to_mem.toM.deq;
            method ToMemMsg#(LdMemRqId#(LLCRqMshrIdx), void) first;
                case(cache.to_mem.toM.first) matches
                    tagged Ld .x: begin
                        let y = x;
                        y.addr = secureRotateAddr(x.addr);
                        return Ld (y);
                    end
                    tagged Wb .x: begin
                        let y = x;
                        y.addr = secureRotateAddr(x.addr);
                        return Wb (y);
                    end
                    default: return ?;
                endcase
            endmethod
        endinterface
        interface rsFromM = cache.to_mem.rsFromM;
    endinterface

    interface Get cRqStuck;
        method ActionValue#(LLCStuck) get;
            let x <- cache.cRqStuck.get;
            let y = x;
            y.addr = secureRotateAddr(x.addr);
            return y;
        endmethod
    endinterface

`else // DISABLE_SECURE_LLC

    interface to_child = cache.to_child;
    interface dma = cache.dma;
    interface to_mem = cache.to_mem;
    interface cRqStuck = cache.cRqStuck;

`endif // DISABLE_SECURE_LLC
`else // SECURITY

    interface to_child = cache.to_child;
    interface dma = cache.dma;
    interface to_mem = cache.to_mem;
    interface cRqStuck = cache.cRqStuck;

`endif // SECURITY

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
