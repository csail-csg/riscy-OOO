import Vector::*;
import Types::*;
import ProcTypes::*;
import ProcSync::*;
import Core::*;
import L1CoCache::*;
import L2Tlb::*;
import CCTypes::*;
import LLBank::*;
import LLCache::*;
import HostDmaIF::*;
import HostDmaLLC::*;
import L1LLConnect::*;
import LLCDmaConnect::*;
import IPIConnect::*;
import DeadlockIF::*;
import DeadlockSync::*;
import RenameDebugIF::*;
import RenameDebugSync::*;

interface Proc;
    // processor request & indication in use, in portal clk domain
    interface ProcReq procReq;
    interface ProcIndInv procIndInv;
    // request & indication inverse, under portal clock domain
    interface HostDmaRequest hostDmaReq;
    method ActionValue#(HostDmaRdData) rdDataToHost;
    method Action wrDoneToHost;
    // LLC ifc to DDR3
    interface LLCMemFifoClient toDDR3;
    // detect deadlock request & indication inverse, under portal clock domain
    interface DeadlockRequest deadlockReq;
    interface DeadlockIndInv deadlockIndInv;
    // rename debug
    interface RenameDebugIndInv renameDebugIndInv;
endinterface

(* synthesize *)
module mkProc#(Clock portalClk, Reset portalRst)(Proc);
    // cores
    Vector#(CoreNum, Core) core = ?;
    for(Integer i = 0; i < valueof(CoreNum); i = i+1) begin
        core[i] <- mkCore(fromInteger(i));
    end

    // proc ind, inv cross clock domain
    Vector#(CoreNum, CoreReq) coreReq = ?;
    Vector#(CoreNum, CoreIndInv) coreIndInv = ?;
    for(Integer i = 0; i < valueof(CoreNum); i = i+1) begin
        coreReq[i] = core[i].coreReq;
        coreIndInv[i] = core[i].coreIndInv;
    end
    ProcReq procReqIfc <- mkProcReqSync(coreReq, portalClk, portalRst);
    ProcIndInv procIndInvIfc <- mkProcIndInvSync(coreIndInv, portalClk, portalRst);

    // DMA from host cross clock domain & reformat req/resp
    HostDmaLLC host <- mkHostDmaLLC(portalClk, portalRst);

    // last level cache
    LLCache llc <- mkLLCache;

    // connect LLC to L1 caches
    Vector#(L1Num, ChildCacheToParent#(L1Way, void)) l1 = ?;
    for(Integer i = 0; i < valueof(CoreNum); i = i+1) begin
        l1[i] = core[i].dCacheToParent;
        l1[i + valueof(CoreNum)] = core[i].iCacheToParent;
    end
    mkL1LLConnect(llc.to_child, l1);

    // connect LLC to DMA
    Vector#(CoreNum, TlbMemClient) tlbToMem = ?;
    for(Integer i = 0; i < valueof(CoreNum); i = i+1) begin
        tlbToMem[i] = core[i].tlbToMem;
    end
    mkLLCDmaConnect(llc.dma, host.to_mem, tlbToMem);

    // connect IPI
    Vector#(CoreNum, CoreIPI) ipi = ?;
    for(Integer i = 0; i < valueof(CoreNum); i = i+1) begin
        ipi[i] = core[i].ipi;
    end
    mkIPIConnect(ipi);

    // deadlock methods cross clock domain
    Vector#(CoreNum, CoreDeadlock) dl = ?;
    for(Integer i = 0; i < valueof(CoreNum); i = i+1) begin
        dl[i] = core[i].deadlock;
    end
    DeadlockSync deadlock <- mkDeadlockSync(dl, llc.cRqStuck, portalClk, portalRst);

    // rename debug methods cross clock domain
    Vector#(CoreNum, CoreRenameDebug) rd;
    for(Integer i = 0; i < valueof(CoreNum); i = i+1) begin
        rd[i] = core[i].renameDebug;
    end
    RenameDebugSync renameDebug <- mkRenameDebugSync(rd, portalClk, portalRst);

    interface procReq = procReqIfc;
    interface procIndInv = procIndInvIfc;
    interface hostDmaReq = host.reqFromHost;
    method rdDataToHost = host.rdDataToHost;
    method wrDoneToHost = host.wrDoneToHost;
    interface toDDR3 = llc.to_mem;
    interface deadlockReq = deadlock.req;
    interface deadlockIndInv = deadlock.indInv;
    interface renameDebugIndInv = renameDebug.indInv;
endmodule
