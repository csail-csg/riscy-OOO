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
