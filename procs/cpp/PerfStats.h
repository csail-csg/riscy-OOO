
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

        // DCache
        init(DCache, L1DLdCnt, "D$ Ld num");
        init(DCache, L1DLdMissCnt, "D$ Ld miss num");
        init(DCache, L1DLdMissLat, "D$ Ld miss total latency");
        init(DCache, L1DStCnt, "D$ St num");
        init(DCache, L1DStMissDataCnt, "D$ St miss data num");
        init(DCache, L1DStMissPermCnt, "D$ St miss permission num");
        init(DCache, L1DStMissLat, "D$ St miss total latency");
        init(DCache, L1DAmoCnt, "D$ Amo num");
        init(DCache, L1DAmoMissDataCnt, "D$ Amo miss data num");
        init(DCache, L1DAmoMissPermCnt, "D$ Amo miss permission num");
        init(DCache, L1DAmoMissLat, "D$ Amo miss total latency");

        // ITlb
        init(ITlb, L1TlbAccessCnt, "ITlb access num");
        init(ITlb, L1TlbMissCnt, "ITlb miss num");
        init(ITlb, L1TlbMissLat, "ITlb miss total latency");

        // DTlb
        init(DTlb, L1TlbAccessCnt, "DTlb access num");
        init(DTlb, L1TlbMissCnt, "DTlb miss num");
        init(DTlb, L1TlbMissLat, "DTlb miss total latency");
        
        // L2Tlb
        init(L2Tlb, L2TlbInstMissCnt, "L2Tlb inst access miss num");
        init(L2Tlb, L2TlbInstMissLat, "L2Tlb inst access miss total latency");
        init(L2Tlb, L2TlbDataMissCnt, "L2Tlb data access miss num");
        init(L2Tlb, L2TlbDataMissLat, "L2Tlb data access miss total latency");

        // DecStage
        init(DecStage, DecRedirectBr, "Dec stage redirect branch num");
        init(DecStage, DecRedirectJmp, "Dec stage redirect jump num");
        init(DecStage, DecRedirectJr, "Dec stage redirect jump reg num");
        init(DecStage, DecRedirectOther, "Dec stage redirect other num");

        // ExeStage
        init(ExeStage, SupRenameCnt, "times of superscalar correct path rename");
        init(ExeStage, ExeRedirectBr, "Exe stage redirect branch num");
        init(ExeStage, ExeRedirectJr, "Exe stage redirect jump reg num");
        init(ExeStage, ExeRedirectOther, "Exe stage redirect other num");
        init(ExeStage, ExeKillLdByLdSt, "Exe stage kill load num (by Ld/St)");
        init(ExeStage, ExeKillLdByCache, "Exe stage kill load num (by cache)");
        init(ExeStage, ExeTlbExcep, "Exe stage TLB exception num");

        // ComStage
        init(ComStage, CycleCnt, "cycles");
        init(ComStage, InstCnt, "instructions");
        init(ComStage, UserInstCnt, "user instructions");
        init(ComStage, SupComUserCnt, "times of superscalar user commit");
        init(ComStage, ComBrCnt, "branch num");
        init(ComStage, ComJmpCnt, "jump num");
        init(ComStage, ComJrCnt, "jump reg num");
        init(ComStage, ComRedirect, "Com stage system inst redirect num");
        init(ComStage, ExcepCnt, "exception num");
        init(ComStage, InterruptCnt, "interrupt num");
        init(ComStage, FlushTlbCnt, "flush TLB num");
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
    }
};

