
// Copyright (c) 2018 Massachusetts Institute of Technology
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
import ClientServer::*;
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
import ProcIF::*;
import BootRomIF::*;
import BootRom::*;
import MemLoaderIF::*;
import MemLoader::*;
import L1LLConnect::*;
import LLCDmaConnect::*;
import MMIOAddrs::*;
import MMIOCore::*;
import MMIOPlatform::*;
import DeadlockIF::*;
import DeadlockSync::*;
import RenameDebugIF::*;
import RenameDebugSync::*;
import DramCommon::*;
import DramLLC::*;
import Performance::*;

interface Proc;
    // processor request & indication in use, in portal clk domain
    interface ProcRequest procReq;
    interface ProcIndInv procIndInv;
    // boot rom request, in portal clock
    interface BootRomRequest bootRomReq;
    interface BootRomIndInv bootRomIndInv;
    // mem loader request & indication inverse, in portal clock
    interface MemLoaderRequest memLoaderReq;
    interface MemLoaderIndInv memLoaderIndInv;
    // to Dram
    interface Client#(DramUserReq, DramUserData) toDram;
    // detect deadlock indication inverse, under portal clock domain
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

    // boot rom
    BootRom bootRom <- mkBootRom(portalClk, portalRst);

    // mem loader
    MemLoader memLoader <- mkMemLoader(portalClk, portalRst);

    // MMIO platform
    Vector#(CoreNum, MMIOCoreToPlatform) mmioToP;
    for(Integer i = 0; i < valueof(CoreNum); i = i+1) begin
        mmioToP[i] = core[i].mmioToPlatform;
    end
    MMIOPlatform mmioPlatform <- mkMMIOPlatform(bootRom.mmio, memLoader.mmio, mmioToP);

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
    mkLLCDmaConnect(llc.dma, memLoader.to_mem, tlbToMem);

    // interface LLC to DRAM and control DRAM latency
    let toDramIfc <- mkDramLLC(llc.to_mem);

    // connect stats
    for(Integer i = 0; i < valueof(CoreNum); i = i+1) begin
        rule broadcastStats;
            Bool doStats <- core[i].sendDoStats;
            for(Integer j = 0; j < valueof(CoreNum); j = j+1) begin
                core[j].recvDoStats(doStats);
            end
            llc.perf.setStatus(doStats);
        endrule
    end

    // proc ind, inv cross clock domain
    Vector#(CoreNum, CoreReq) coreReq = ?;
    Vector#(CoreNum, CoreIndInv) coreIndInv = ?;
    for(Integer i = 0; i < valueof(CoreNum); i = i+1) begin
        coreReq[i] = core[i].coreReq;
        coreIndInv[i] = core[i].coreIndInv;
    end
    ProcRequest procReqIfc <- mkProcReqSync(
        coreReq, mmioPlatform, llc, portalClk, portalRst
    );
    ProcIndInv procIndInvIfc <- mkProcIndInvSync(
        coreIndInv, mmioPlatform, llc, portalClk, portalRst
    );

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
    interface bootRomReq = bootRom.hostReq;
    interface bootRomIndInv = bootRom.hostIndInv;
    interface memLoaderReq = memLoader.hostReq;
    interface memLoaderIndInv = memLoader.hostIndInv;
    interface toDram = toDramIfc;
    interface deadlockIndInv = deadlock.indInv;
    interface renameDebugIndInv = renameDebug.indInv;
endmodule
