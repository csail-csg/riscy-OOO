
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

import Vector::*;
import Types::*;
import ProcTypes::*;
import ProcSync::*;
import Core::*;
import L1CoCache::*;
import L2Tlb::*;
import CCTypes::*;
import CacheUtils::*;
import LLBank::*;
import LLCache::*;
import HostDmaIF::*;
import HostDmaLLC::*;
import L1LLConnect::*;
import LLCDmaConnect::*;
import MMIOAddrs::*;
import MMIOCore::*;
import MMIOPlatform::*;
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
    // LLC ifc to Dram
    interface LLCMemFifoClient toDram;
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

    // MMIO platform
    Vector#(CoreNum, MMIOCoreToPlatform) mmioToP;
    for(Integer i = 0; i < valueof(CoreNum); i = i+1) begin
        mmioToP[i] = core[i].mmioToPlatform;
    end
    MMIOPlatform mmioPlatform <- mkMMIOPlatform(mmioToP);

    // proc ind, inv cross clock domain
    Vector#(CoreNum, CoreReq) coreReq = ?;
    Vector#(CoreNum, CoreIndInv) coreIndInv = ?;
    for(Integer i = 0; i < valueof(CoreNum); i = i+1) begin
        coreReq[i] = core[i].coreReq;
        coreIndInv[i] = core[i].coreIndInv;
    end
    ProcReq procReqIfc <- mkProcReqSync(
        coreReq, mmioPlatform, portalClk, portalRst
    );
    ProcIndInv procIndInvIfc <- mkProcIndInvSync(
        coreIndInv, mmioPlatform, portalClk, portalRst
    );

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

    // connect stats
    for(Integer i = 0; i < valueof(CoreNum); i = i+1) begin
        rule broadcastStats;
            Bool doStats <- core[i].sendDoStats;
            for(Integer j = 0; j < valueof(CoreNum); j = j+1) begin
                core[j].recvDoStats(doStats);
            end
        endrule
    end

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

    // when requesting DRAM, we need to subtract the main mem base from the
    // requesting addr
    interface MemFifoClient toDram;
        interface FifoDeq toM;
            method notEmpty = llc.to_mem.toM.notEmpty;
            method deq = llc.to_mem.toM.deq;
            method ToMemMsg#(LdMemRqId#(LLCRqMshrIdx), void) first;
                Addr dramBase = {mainMemBaseAddr, 3'b0};
                case(llc.to_mem.toM.first) matches
                    tagged Ld .r: begin
                        return Ld (LdMemRq {
                            addr: r.addr - dramBase,
                            child: r.child,
                            id: r.id
                        });
                    end
                    tagged Wb .r: begin
                        return Wb (WbMemRs {
                            addr: r.addr - dramBase,
                            byteEn: r.byteEn,
                            data: r.data
                        });
                    end
                    default: return ?;
                endcase
            endmethod
        endinterface
        interface rsFromM = llc.to_mem.rsFromM;
    endinterface

    interface deadlockReq = deadlock.req;
    interface deadlockIndInv = deadlock.indInv;
    interface renameDebugIndInv = renameDebug.indInv;
endmodule
