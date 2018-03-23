import CCTypes::*;
import Core::*;
import L1CoCache::*;
import L2Tlb::*;
import MMIOCore::*;

// get rid of debug ifc
interface CoreTop;
    // core request & indication
    interface CoreReq coreReq;
    interface CoreIndInv coreIndInv;
    // coherent caches to LLC
    interface ChildCacheToParent#(L1Way, void) dCacheToParent;
    interface ChildCacheToParent#(L1Way, void) iCacheToParent;
    // DMA to LLC
    interface TlbMemClient tlbToMem;
    // MMIO
    interface MMIOCoreToPlatform mmioToPlatform;
endinterface

(* synthesize *)
module mkCoreTop(CoreTop);
    Core core <- mkCore(0);

    interface coreReq = core.coreReq;
    interface coreIndInv = core.coreIndInv;
    interface dCacheToParent = core.dCacheToParent;
    interface iCacheToParent = core.iCacheToParent;
    interface tlbToMem = core.tlbToMem;
    interface mmioToPlatform = core.mmioToPlatform;
endmodule
