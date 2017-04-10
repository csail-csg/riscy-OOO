`include "ProcConfig.bsv"

import Vector::*;
import ClientServer::*;
import GetPut::*;
import Assert::*;

import CacheUtils::*;
import Types::*;
import ProcTypes::*;
import Performance::*;
import Fifo::*;
import CCTypes::*;
import L1Pipe::*;
import L1CRqMshr::*;
import L1PRqMshr::*;
import L1Bank::*;
import ICRqMshr::*;
import IPRqMshr::*;
import IBank::*;

export L1Num;
export L1WayNum;
export L1Way;

export DProcReqId;
export DCoCache(..);
export mkDCoCache;

export ISupSz;
export ICoCache(..);
export mkICoCache;

// L1$: 1 I$ and 1 D$
typedef TMul#(CoreNum, 2) L1Num;

// Way num is shared among all coherent L1$ (I and D)
typedef 4 L1WayNum;
typedef Bit#(TLog#(L1WayNum)) L1Way;


////////
// D$ //
////////
// 32KB
typedef `LOG_DCACHE_BANKS LgDBankNum;
typedef TSub#(7, LgDBankNum) LgDSetNum;

typedef Bit#(LgDBankNum) DBankId;
typedef LgDSetNum DIndexSz;
typedef Bit#(DIndexSz) DIndex;
typedef GetTagSz#(LgDBankNum, LgDSetNum) DTagSz;
typedef Bit#(DTagSz) DTag;

typedef 4 DCRqNum;
typedef 2 DPRqNum;
typedef Bit#(TLog#(DCRqNum)) DCRqMshrIdx;
typedef Bit#(TLog#(DPRqNum)) DPRqMshrIdx;

typedef Bit#(TMax#(SizeOf#(LdStQTag), SizeOf#(SBIndex))) DProcReqId;

(* synthesize *)
module mkDCRqMshrWrapper(
    L1CRqMshr#(DCRqNum, L1Way, DTag, ProcRq#(DProcReqId))
);
    function Addr getAddrFromReq(ProcRq#(DProcReqId) r);
        return r.addr;
    endfunction
    let m <- mkL1CRqMshr(getAddrFromReq);
    return m;
endmodule

(* synthesize *)
module mkDPRqMshrWrapper(
    L1PRqMshr#(DPRqNum)
);
    let m <- mkL1PRqMshr;
    return m;
endmodule

(* synthesize *)
module mkDPipeline(
    L1Pipe#(LgDBankNum, L1WayNum, DIndex, DTag, DCRqMshrIdx, DPRqMshrIdx)
);
    let m <- mkL1Pipe;
    return m;
endmodule

typedef L1Bank#(LgDBankNum, L1WayNum, DIndexSz, DTagSz, DCRqNum, DPRqNum, DProcReqId) DCacheWrapper;

module mkDCacheWrapper#(L1ProcResp#(DProcReqId) procResp)(DCacheWrapper);
    let m <- mkL1Cache(mkDCRqMshrWrapper, mkDPRqMshrWrapper, mkDPipeline, procResp);
    return m;
endmodule

interface DCoCache;
    interface L1ProcReq#(DProcReqId) procReq;
    method Action flush;
    method Bool flush_done;
    method Action resetLinkAddr;
    interface Perf#(L1PerfType) perf;

    interface ChildCacheToParent#(L1Way, void) to_parent;

    // detect deadlock: only in use when macro CHECK_DEADLOCK is defined
    interface Get#(L1CRqStuck) cRqStuck;
    interface Get#(L1PRqStuck) pRqStuck;
endinterface

module mkDCoCache#(L1ProcResp#(DProcReqId) procResp)(DCoCache);
    let cache <- mkDCacheWrapper(procResp);

    // TODO perf counters
    Fifo#(1, L1PerfType) perfReqQ <- mkCFFifo;
`ifdef PERF_COUNT
    Fifo#(1, PerfResp#(L1PerfType)) perfRespQ <- mkCFFifo;

    rule doPerf;
        let t <- toGet(perfReqQ).get;
        let d = cache.getPerfData(t);
        perfRespQ.enq(PerfResp {
            pType: t,
            data: d
        });
    endrule
`endif

    interface procReq = cache.procReq;

    method flush = noAction;
    method flush_done = True;

    method Action resetLinkAddr;
        cache.resetLinkAddr;
    endmethod

    interface Perf perf;
        method Action setStatus(Bool stats);
            cache.setPerfStatus(stats);
        endmethod
        method Action req(L1PerfType r);
            perfReqQ.enq(r);
        endmethod
        method ActionValue#(PerfResp#(L1PerfType)) resp;
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

    interface to_parent = cache.to_parent;

    interface cRqStuck = cache.cRqStuck;
    interface pRqStuck = cache.pRqStuck;
endmodule


////////
// I$ //
////////
typedef 0 LgIBankNum;
typedef 7 LgISetNum;

typedef Bit#(LgIBankNum) IBankId;
typedef LgISetNum IIndexSz;
typedef Bit#(IIndexSz) IIndex;
typedef GetTagSz#(LgIBankNum, LgISetNum) ITagSz;
typedef Bit#(ITagSz) ITag;

typedef 4 ICRqNum;
typedef 2 IPRqNum;
typedef Bit#(TLog#(ICRqNum)) ICRqMshrIdx;
typedef Bit#(TLog#(IPRqNum)) IPRqMshrIdx;

typedef SupSize ISupSz;

(* synthesize *)
module mkICRqMshrWrapper(
    ICRqMshr#(ICRqNum, L1Way, ITag, ProcRqToI, Vector#(ISupSz, Maybe#(Instruction)))
);
    function Addr getAddrFromReq(ProcRqToI r);
        return r.addr;
    endfunction
    let m <- mkICRqMshr(getAddrFromReq);
    return m;
endmodule

(* synthesize *)
module mkIPRqMshrWrapper(
    IPRqMshr#(IPRqNum)
);
    let m <- mkIPRqMshr;
    return m;
endmodule

(* synthesize *)
module mkIPipeline(
    L1Pipe#(LgIBankNum, L1WayNum, IIndex, ITag, ICRqMshrIdx, IPRqMshrIdx)
);
    let m <- mkL1Pipe;
    return m;
endmodule

typedef IBank#(ISupSz, LgIBankNum, L1WayNum, IIndexSz, ITagSz, ICRqNum, IPRqNum) IBankWrapper;

(* synthesize *)
module mkIBankWrapper(IBankWrapper);
    let m <- mkIBank(mkICRqMshrWrapper, mkIPRqMshrWrapper, mkIPipeline);
    return m;
endmodule

interface ICoCache;
    interface Server#(Addr, Vector#(ISupSz, Maybe#(Instruction))) to_proc;
    method Action flush;
    method Bool flush_done;
    interface Perf#(L1PerfType) perf;

    interface ChildCacheToParent#(L1Way, void) to_parent;

    // detect deadlock: only in use when macro CHECK_DEADLOCK is defined
    interface Get#(ICRqStuck) cRqStuck;
    interface Get#(IPRqStuck) pRqStuck;
endinterface

(* synthesize *)
module mkICoCache(ICoCache);
`ifdef DEBUG_ICACHE
    staticAssert(False, "DEBUG_ICACHE should not be defined");
`endif

    let cache <- mkIBankWrapper;

    Fifo#(1, L1PerfType) perfReqQ <- mkCFFifo;
`ifdef PERF_COUNT
    Fifo#(1, PerfResp#(L1PerfType)) perfRespQ <- mkCFFifo;

    rule doPerf;
        let t <- toGet(perfReqQ).get;
        let d = cache.getPerfData(t);
        perfRespQ.enq(PerfResp {
            pType: t,
            data: d
        });
    endrule
`endif

    interface Server to_proc;
        interface request = cache.to_proc.req;
        interface response = cache.to_proc.resp;
    endinterface
    method Action flush = noAction;
    method Bool flush_done = True;

    interface Perf perf;
        method Action setStatus(Bool stats);
            cache.setPerfStatus(stats);
        endmethod
        method Action req(L1PerfType r);
            perfReqQ.enq(r);
        endmethod
        method ActionValue#(PerfResp#(L1PerfType)) resp;
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

    interface to_parent = cache.to_parent;

    interface cRqStuck = cache.cRqStuck;
    interface pRqStuck = cache.pRqStuck;
endmodule
