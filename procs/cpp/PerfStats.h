
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
private:
    typedef std::map<PerfIndex, PerfCounter, PerfIndexLess> CntMap;
    CntMap cnts; // counters

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

        // create counter mapping
        cnts.clear();

        // ICache
        init(ICache, LdCnt, "I$ Ld num");
        init(ICache, LdMissCnt, "I$ Ld miss num");
        //init(ICache, LdMissLat, "I$ Ld miss total latency");

        // DCache
        init(DCache, LdCnt, "D$ Ld num");
        init(DCache, LdMissCnt, "D$ Ld miss num");
        //init(DCache, LdMissLat, "D$ Ld miss total latency");
        init(DCache, StCnt, "D$ St num");
        init(DCache, StMissCnt, "D$ St miss num");
        //init(DCache, StMissLat, "D$ St miss total latency");
        init(DCache, AmoCnt, "D$ Amo num");
        init(DCache, AmoMissCnt, "D$ Amo miss num");
        //init(DCache, AmoMissLat, "D$ Amo miss total latency");

        // ITlb
        init(ITlb, TlbAccessCnt, "ITlb access num");
        init(ITlb, TlbMissCnt, "ITlb miss num");
        init(ITlb, TlbMissLat, "ITlb miss total latency");
        init(ITlb, TlbFlushCnt, "ITlb flush num");

        // DTlb
        init(DTlb, TlbAccessCnt, "DTlb access num");
        init(DTlb, TlbMissCnt, "DTlb miss num");
        init(DTlb, TlbMissLat, "DTlb miss total latency");
        init(DTlb, TlbFlushCnt, "DTlb flush num");

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
        init(ExeStage, ExeKillLd, "Exe stage kill load num");
        init(ExeStage, ExeTlbExcep, "Exe stage TLB exception num");
        init(ExeStage, HtifStallCnt, "HTIF stall num");
        init(ExeStage, HtifStallLat, "HTIF stall total latency");

        // ComStage
        init(ComStage, CycleCnt, "cycles");
        init(ComStage, InstCnt, "instructions");
        init(ComStage, UserInstCnt, "user instructions");
        init(ComStage, SupComUserCnt, "times of superscalar user commit");
        init(ComStage, ComBrCnt, "branch num");
        init(ComStage, ComJmpCnt, "jump num");
        init(ComStage, ComJrCnt, "jump reg num");
        init(ComStage, ComRedirect, "Com stage redirect num");
        init(ComStage, TrapCnt, "trap num");
        init(ComStage, SretCnt, "sret num");
        init(ComStage, MrtsCnt, "mrts num");
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
