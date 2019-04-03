
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

#pragma once

#include <map>
#include "GeneratedTypes.h"
#include <string>
#include <stdio.h>
#include <semaphore.h>
#include <functional>

class PerfIndex {
public:
    PerfLocation loc;
    PerfType type;
    PerfIndex() {}
    PerfIndex(PerfLocation l, PerfType t) : loc(l), type(t) {}
};

class PerfIndexLess {
public:
    bool operator()(const PerfIndex &x, const PerfIndex &y) const {
        return x.loc < y.loc || (x.loc == y.loc && x.type < y.type);
    }
};

struct PerfCounter {
    uint64_t data;
    std::string name;
    bool valid;
    PerfCounter() : valid(false) {}
    PerfCounter(uint64_t d, const char *s) : data(d), name(s), valid(false) {}
    void print(FILE *fp) {
        if(valid) {
            fprintf(fp, "%s = %lld\n", name.c_str(), (long long unsigned) data);
        }
        else {
            fprintf(fp, "%s = invalid\n", name.c_str());
        }
    }
    void reset() {
        data = 0;
        valid = false;
    }
};

class PerfStats {
protected:
    typedef std::map<PerfIndex, PerfCounter, PerfIndexLess> CntMap;
    CntMap cnts; // all counters

    uint8_t cur_core; // current core being queried

    // sem for getting perf values
    sem_t query_sem;

    // function to send req for perf counters
    std::function<void(uint8_t, PerfLocation, PerfType)> req_perf;

    void init(PerfLocation loc, PerfType type, const char *str) {
        PerfIndex idx(loc, type);
        cnts[idx] = PerfCounter(0, str);
    }

public:
    PerfStats() {
        // init sem
        sem_init(&query_sem, 0, 0);
        // init counter mapping
        cnts.clear();
    }

    void set_req_perf(std::function<void(uint8_t, PerfLocation, PerfType)> f) {
        req_perf = f;
    }

    void get_all_perf(uint32_t core) {
        cur_core = core;
        for(CntMap::iterator it = cnts.begin(); it != cnts.end(); it++) {
            req_perf(cur_core, (it->first).loc, (it->first).type);
            sem_wait(&query_sem); // waiting for resp
        }
    }

    void inform_resp(uint8_t core, PerfLocation loc, PerfType type, uint64_t data) {
        if(core != cur_core) {
            fprintf(stderr, "[ERROR] getting perf resp from wrong core %d, expecting core %d\n",
                    int(core), int(cur_core));
            return;
        }
        CntMap::iterator it = cnts.find(PerfIndex(loc, type));
        if(it != cnts.end()) {
            // record data
            (it->second).data = data;
            (it->second).valid = true;
            // wake up get_all_perf()
            sem_post(&query_sem);
        }
        else {
            fprintf(stderr, "[ERROR] getting wrong perf resp: %d, %d, %lld\n", loc, type, (long long unsigned)data);
        }
    }

    void print(FILE *fp) {
        for(CntMap::iterator it = cnts.begin(); it != cnts.end(); it++) {
            (it->second).print(fp);
        }
    }

    void reset() {
        for(CntMap::iterator it = cnts.begin(); it != cnts.end(); it++) {
            (it->second).reset();
        }
    }
};

class CorePerfStats : public PerfStats {
public:
    CorePerfStats() {
        // ICache
        init(ICache, L1ILdCnt, "I$ Ld num");
        init(ICache, L1ILdMissCnt, "I$ Ld miss num");
        init(ICache, L1ILdMissLat, "I$ Ld miss total latency");
        init(ICache, L1IReconcileCnt, "I$ reconcile num");

        // DCache
        init(DCache, L1DLdCnt, "D$ Ld num");
        init(DCache, L1DLdMissCnt, "D$ Ld miss num");
        init(DCache, L1DLdMissLat, "D$ Ld miss total latency");
        init(DCache, L1DStCnt, "D$ St num");
        init(DCache, L1DStMissCnt, "D$ St miss num");
        init(DCache, L1DStMissLat, "D$ St miss total latency");
        init(DCache, L1DAmoCnt, "D$ Amo num");
        init(DCache, L1DAmoMissCnt, "D$ Amo miss num");
        init(DCache, L1DAmoMissLat, "D$ Amo miss total latency");
        init(DCache, L1DStPrefetchCnt, "D$ StPrefetch num");
        init(DCache, L1DStPrefetchMissCnt, "D$ StPrefetch miss num");
        init(DCache, L1DSelfInvCnt, "D$ self inv num");
        init(DCache, L1DReconcileCnt, "D$ reconcile num");

        // ITlb
        init(ITlb, L1TlbAccessCnt, "ITlb access num");
        init(ITlb, L1TlbMissParentCnt, "ITlb miss parent num");
        init(ITlb, L1TlbMissParentLat, "ITlb miss parent total latency");

        // DTlb
        init(DTlb, L1TlbAccessCnt, "DTlb access num");
        init(DTlb, L1TlbMissParentCnt, "DTlb miss parent num");
        init(DTlb, L1TlbMissParentLat, "DTlb miss parent total latency");
        init(DTlb, L1TlbMissPeerCnt, "DTlb miss peer num");
        init(DTlb, L1TlbMissPeerLat, "DTlb miss peer total latency");
        init(DTlb, L1TlbHitUnderMissCnt, "DTlb hit under miss num");
        init(DTlb, L1TlbAllMissCycles, "DTlb blocked by miss cycles");
        
        // L2Tlb
        init(L2Tlb, L2TlbInstMissCnt, "L2Tlb inst access miss num");
        init(L2Tlb, L2TlbInstMissLat, "L2Tlb inst access miss total latency");
        init(L2Tlb, L2TlbInstPageWalks, "L2Tlb inst page walk num");
        init(L2Tlb, L2TlbInstSavedPageWalks, "L2Tlb inst saved page walk num");
        init(L2Tlb, L2TlbInstHugePageHits, "L2Tlb inst huge page hit num");
        init(L2Tlb, L2TlbInstHugePageMisses, "L2Tlb inst huge page miss num");
        init(L2Tlb, L2TlbDataMissCnt, "L2Tlb data access miss num");
        init(L2Tlb, L2TlbDataMissLat, "L2Tlb data access miss total latency");
        init(L2Tlb, L2TlbDataPageWalks, "L2Tlb data page walk num");
        init(L2Tlb, L2TlbDataSavedPageWalks, "L2Tlb data saved page walk num");
        init(L2Tlb, L2TlbDataHugePageHits, "L2Tlb data huge page hit num");
        init(L2Tlb, L2TlbDataHugePageMisses, "L2Tlb data huge page miss num");
        init(L2Tlb, L2TlbHitUnderMissCnt, "L2Tlb hit under miss num");
        init(L2Tlb, L2TlbAllMissCycles, "L2Tlb blocked by page walk cycles");
        init(L2Tlb, L2TlbPeerSavedMemReqs, "L2Tlb peer saved mem req num");

        // DecStage
        init(DecStage, DecRedirectBr, "Dec stage redirect branch num");
        init(DecStage, DecRedirectJmp, "Dec stage redirect jump num");
        init(DecStage, DecRedirectJr, "Dec stage redirect jump reg num");
        init(DecStage, DecRedirectOther, "Dec stage redirect other num");

        // ExeStage
        init(ExeStage, SupRenameCnt, "times of superscalar correct path rename");
        init(ExeStage, SpecNoneCycles, "Cycles of none speculate");
        init(ExeStage, SpecNonMemCycles, "Cycles of non-mem speculate");
        init(ExeStage, ExeRedirectBr, "Exe stage redirect branch num");
        init(ExeStage, ExeRedirectJr, "Exe stage redirect jump reg num");
        init(ExeStage, ExeRedirectOther, "Exe stage redirect other num");
        init(ExeStage, ExeIntMulCnt, "Exe stage int mul num");
        init(ExeStage, ExeIntDivCnt, "Exe stage int div num");
        init(ExeStage, ExeFpFmaCnt, "Exe stage fp add/mul/fma num");
        init(ExeStage, ExeFpDivCnt, "Exe stage fp div num");
        init(ExeStage, ExeFpSqrtCnt, "Exe stage fp sqrt num");

        // MemStage
        init(MemStage, ExeLdStallByLd, "Exe stage stall load num by LdQ");
        init(MemStage, ExeLdStallBySt, "Exe stage stall load num by StQ");
        init(MemStage, ExeLdStallBySB, "Exe stage stall load num by SB");
        init(MemStage, ExeLdForward, "Exe stage load forward num");
        init(MemStage, ExeLdMemLat, "Exe stage load mem total latency");
        init(MemStage, ExeStMemLat, "Exe stage store mem total latency");
        init(MemStage, ExeLdToUseLat, "Exe stage load to use total latency");
        init(MemStage, ExeLdToUseCnt, "Exe stage load to use num");
        init(MemStage, ExeTlbExcep, "Exe stage TLB exception num");
        init(MemStage, ExeScSuccessCnt, "Exe stage sc success num");
        init(MemStage, ExeLrScAmoAcqCnt, "Exe stage lr/sc/amo acquire num");
        init(MemStage, ExeLrScAmoRelCnt, "Exe stage lr/sc/amo release num");
        init(MemStage, ExeFenceAcqCnt, "Exe stage fence acquire num");
        init(MemStage, ExeFenceRelCnt, "Exe stage fence release num");
        init(MemStage, ExeFenceCnt, "Exe stage fence num");
        init(MemStage, ExeUserLrScAmoAcqCnt, "Exe stage user lr/sc/amo acquire num");
        init(MemStage, ExeUserLrScAmoRelCnt, "Exe stage user lr/sc/amo release num");
        init(MemStage, ExeUserFenceAcqCnt, "Exe stage user fence acquire num");
        init(MemStage, ExeUserFenceRelCnt, "Exe stage user fence release num");
        init(MemStage, ExeUserFenceCnt, "Exe stage user fence num");
        init(MemStage, ExeDropStPrefetchCnt, "Exe stage drop store prefetch num");

        // ComStage
        init(ComStage, CycleCnt, "cycles");
        init(ComStage, UserCycleCnt, "user cycles");
        init(ComStage, InstCnt, "instructions");
        init(ComStage, UserInstCnt, "user instructions");
        init(ComStage, SupComUserCnt, "times of superscalar user commit");
        init(ComStage, ComBrCnt, "branch num");
        init(ComStage, ComJmpCnt, "jump num");
        init(ComStage, ComJrCnt, "jump reg num");
        init(ComStage, ComLdCnt, "load num");
        init(ComStage, ComStCnt, "store num");
        init(ComStage, ComLrCnt, "load reserve num");
        init(ComStage, ComScCnt, "store conditional num");
        init(ComStage, ComAmoCnt, "amo num");
        init(ComStage, ComLdKillByLd, "Com stage kill load num by Ld");
        init(ComStage, ComLdKillBySt, "Com stage kill load num by St");
        init(ComStage, ComLdKillByCache, "Com stage kill load num by cache");
        init(ComStage, ComSysCnt, "Com stage system inst num");
        init(ComStage, ComEcallCnt, "Com stage ecall num");
        init(ComStage, ExcepCnt, "exception num");
        init(ComStage, InterruptCnt, "interrupt num");
        init(ComStage, FlushTlbCnt, "flush TLB num");
        init(ComStage, FlushSecurityCnt, "flush security num");
        init(ComStage, FlushBPCnt, "flush branch predictor num");
        init(ComStage, FlushCacheCnt, "flush L1 caches num");

        // CoreSize
        init(CoreSize, LdQFullCycles, "LdQ full cycles");
        init(CoreSize, StQFullCycles, "StQ full cycles");
        init(CoreSize, ROBFullCycles, "ROB full cycles");
        init(CoreSize, AluRS0FullCycles, "alu RS 0 full cycles");
        init(CoreSize, AluRS1FullCycles, "alu RS 1 full cycles");
        init(CoreSize, FpuMulDivRSFullCycles, "fpu/mul/div RS full cycles");
        init(CoreSize, MemRSFullCycles, "mem RS full cycles");
        init(CoreSize, EpochFullCycles, "epoch full cycles");
        init(CoreSize, SpecTagFullCycles, "spec tag full cycles");
    }
};

class UncorePerfStats : public PerfStats {
public:
    UncorePerfStats() {
        // LLC
        init(LLC, LLCDmaMemLdCnt, "LLC DMA mem load num");
        init(LLC, LLCDmaMemLdLat, "LLC DMA mem load total latency");
        init(LLC, LLCNormalMemLdCnt, "LLC normal mem load num");
        init(LLC, LLCNormalMemLdLat, "LLC normal mem load total latency");
        init(LLC, LLCDmaMemStCnt, "LLC DMA mem store num");
        init(LLC, LLCNormalMemStCnt, "LLC normal mem store num");
        init(LLC, LLCMshrBlockCycles, "LLC mshr block cycles");
        init(LLC, LLCDownRespCnt, "LLC downgrade resp num");
        init(LLC, LLCDownRespDataCnt, "LLC downgrade resp data num");
        init(LLC, LLCDownReqCnt, "LLC downgrade req num");
        init(LLC, LLCUpRespCnt, "LLC upgrade resp num");
        init(LLC, LLCUpRespDataCnt, "LLC upgrade resp data num");
        init(LLC, LLCDmaLdReqCnt, "LLC dma load req num");
        init(LLC, LLCDmaStReqCnt, "LLC dma store req num");
    }
};

