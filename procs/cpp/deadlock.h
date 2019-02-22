
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
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include "GeneratedTypes.h"
#include "DeadlockIndication.h"

class DeadlockIndication : public DeadlockIndicationWrapper {
private:
    FILE *log_fp;
    long long unsigned ignore_user_commit_stucks;

public:
    DeadlockIndication(int id, long long unsigned ignore_user_stucks) :
        DeadlockIndicationWrapper(id),
        log_fp(0),
        ignore_user_commit_stucks(ignore_user_stucks) {
        log_fp = fopen("deadlock.txt", "w");
    }

    virtual ~DeadlockIndication() {
        if(log_fp != 0) {
            fclose(log_fp);
            log_fp = 0;
        }
    }

    virtual void dCacheCRqStuck(uint8_t core, const uint64_t addr, const uint8_t op,
                                const uint8_t state, const uint8_t slotCs, const int waitP ) {
        fprintf(stderr, "  [DEADLOCK dCacheCRqStuck] core %d, addr %016llx, op %d, state %d, slotCs %d, waitP %d\n",
                (int)core, (long long unsigned)addr, (int)op, (int)state, (int)slotCs, waitP);
        fprintf(log_fp, "[DEADLOCK dCacheCRqStuck] core %d, addr %016llx, op %d, state %d, slotCs %d, waitP %d\n",
                (int)core, (long long unsigned)addr, (int)op, (int)state, (int)slotCs, waitP);
        fflush(log_fp);
    }

    virtual void dCachePRqStuck(uint8_t core, const uint64_t addr, const uint8_t toState, const uint8_t state) {
        fprintf(stderr, "  [DEADLOCK dCachePRqStuck] core %d, addr %016llx, toState %d, state %d\n",
                (int)core, (long long unsigned)addr, (int)toState, (int)state);
        fprintf(log_fp, "[DEADLOCK dCachePRqStuck] core %d, addr %016llx, toState %d, state %d\n",
                (int)core, (long long unsigned)addr, (int)toState, (int)state);
        fflush(log_fp);
    }

    virtual void iCacheCRqStuck(uint8_t core, const uint64_t addr, const uint8_t state, const int waitP) {
        fprintf(stderr, "  [DEADLOCK iCacheCRqStuck] core %d, addr %016llx, state %d, waitP %d\n",
                (int)core, (long long unsigned)addr, (int)state, waitP);
        fprintf(log_fp, "[DEADLOCK iCacheCRqStuck] core %d, addr %016llx, state %d, waitP %d\n",
                (int)core, (long long unsigned)addr, (int)state, waitP);
        fflush(log_fp);
    }

    virtual void iCachePRqStuck(uint8_t core, const uint64_t addr, const uint8_t toState, const uint8_t state) {
        fprintf(stderr, "  [DEADLOCK iCachePRqStuck] core %d, addr %016llx, toState %d, state %d\n",
                (int)core, (long long unsigned)addr, (int)toState, (int)state);
        fprintf(log_fp, "[DEADLOCK iCachePRqStuck] core %d, addr %016llx, toState %d, state %d\n",
                (int)core, (long long unsigned)addr, (int)toState, (int)state);
        fflush(log_fp);
    }

    virtual void llcCRqStuck(const LLCStuckSrc src, const uint8_t childId,
                             const uint64_t addr, const uint8_t fromState,
                             const uint8_t toState, const uint8_t state, const int waitP,
                             const uint32_t dirPendTag, const uint32_t dirPendState) {
        fprintf(stderr, "  [DEADLOCK llcCRqStuck] src %d, child %d, addr %016llx, from %d, to %d, "
                "state %d, waitP %d, dirPendTag %08x, dirPendState %08x\n",
                (int)src, (int)childId, (long long unsigned)addr, (int)fromState, (int)toState, (int)state,
                waitP, (int)dirPendTag, (int)dirPendState);
        fprintf(log_fp, "[DEADLOCK llcCRqStuck] src %d, child %d, addr %016llx, from %d, to %d, "
                "state %d, waitP %d, dirPendTag %08x, dirPendState %08x\n",
                (int)src, (int)childId, (long long unsigned)addr, (int)fromState, (int)toState, (int)state,
                waitP, (int)dirPendTag, (int)dirPendState);
        fflush(log_fp);
    }

    virtual void checkStarted(uint8_t core) {
        fprintf(stderr, "  [DEADLOCK check started]\n");
        fprintf(log_fp, "[DEADLOCK check started]\n");
        fflush(log_fp);
    }

    virtual void renameInstStuck(uint8_t core, const uint64_t pc, const uint8_t fetchMainEp,
                                 const int fetchWaitRedirect, const int fetchWaitFlush,
                                 const uint8_t emCurEp, const uint8_t emCheckedEp) {
        fprintf(stderr, "  [DEADLOCK renameInstStuck] core %d, pc %016llx, fetchMainEp %d, "
                "fetchWaitRedirect %d, fetchWaitFlush %d, "
                "emCurEp %d, emCheckedEp %d\n",
                (int)core, (long long unsigned)pc, (int)fetchMainEp,
                fetchWaitRedirect, fetchWaitFlush, (int)emCurEp,
                (int)emCheckedEp);
        fprintf(log_fp, "[DEADLOCK renameInstStuck] core %d, pc %016llx, fetchMainEp %d, "
                "fetchWaitRedirect %d, fetchWaitFlush %d, "
                "emCurEp %d, emCheckedEp %d\n",
                (int)core, (long long unsigned)pc, (int)fetchMainEp,
                fetchWaitRedirect, fetchWaitFlush, (int)emCurEp,
                (int)emCheckedEp);
        fflush(log_fp);
    }

    virtual void renameCorrectPathStuck(uint8_t core, const uint64_t pc, const uint8_t fetchMainEp,
                                        const int fetchWaitRedirect, const int fetchWaitFlush,
                                        const uint8_t emCurEp, const uint8_t emCheckedEp) {
        fprintf(stderr, "  [DEADLOCK renameCorrectPathStuck] core %d, pc %016llx, fetchMainEp %d, "
                "fetchWaitRedirect %d, fetchWaitFlush %d, "
                "emCurEp %d, emCheckedEp %d\n",
                (int)core, (long long unsigned)pc, (int)fetchMainEp,
                fetchWaitRedirect, fetchWaitFlush, (int)emCurEp,
                (int)emCheckedEp);
        fprintf(log_fp, "[DEADLOCK renameCorrectPathStuck] core %d, pc %016llx, fetchMainEp %d, "
                "fetchWaitRedirect %d, fetchWaitFlush %d, "
                "emCurEp %d, emCheckedEp %d\n",
                (int)core, (long long unsigned)pc, (int)fetchMainEp,
                fetchWaitRedirect, fetchWaitFlush, (int)emCurEp,
                (int)emCheckedEp);
        fflush(log_fp);
    }

    virtual void commitInstStuck(const uint8_t core, const uint64_t pc,
                                 const uint8_t iType, const int isException,
                                 const int isInterrupt, const uint8_t trapVal,
                                 const uint8_t state, const int claimedPhyReg,
                                 const int ldKilled, const int memAccessAtCommit,
                                 const int lsqAtCommitNotified, const int nonMMIOStDone,
                                 const int epochIncremented, const uint32_t specBits,
                                 const int stbEmpty, const int stqEmpty,
                                 const int tlbNoPendingReq, const uint8_t prv,
                                 const uint64_t instCount) {
        fprintf(stderr, "  [DEADLOCK commitInstStuck] core %d, pc %016llx, iType %d, isException %d, "
                "isInterrupt %d, trapVal %d, state %d, claimedPhyReg %d, ldKilled %d, "
                "memAccessAtCommit %d, lsqAtCommitNotified %d, nonMMIOStDone %d, epochIncremented %d, "
                "specBits %08x, stbEmpty %d, stqEmpty %d, tlbNoPendingReq %d, prv %d, instCount %llu\n",
                (int)core, (long long unsigned)pc, (int)iType, isException, isInterrupt, (int)trapVal,
                (int)state, claimedPhyReg, ldKilled, memAccessAtCommit, lsqAtCommitNotified,
                nonMMIOStDone, epochIncremented, (unsigned)specBits, stbEmpty, stqEmpty,
                tlbNoPendingReq, (int)prv, (long long unsigned)instCount);
        fprintf(log_fp, "  [DEADLOCK commitInstStuck] core %d, pc %016llx, iType %d, isException %d, "
                "isInterrupt %d, trapVal %d, state %d, claimedPhyReg %d, ldKilled %d, "
                "memAccessAtCommit %d, lsqAtCommitNotified %d, nonMMIOStDone %d, epochIncremented %d, "
                "specBits %08x, stbEmpty %d, stqEmpty %d, tlbNoPendingReq %d, prv %d, instCount %llu\n",
                (int)core, (long long unsigned)pc, (int)iType, isException, isInterrupt, (int)trapVal,
                (int)state, claimedPhyReg, ldKilled, memAccessAtCommit, lsqAtCommitNotified,
                nonMMIOStDone, epochIncremented, (unsigned)specBits, stbEmpty, stqEmpty,
                tlbNoPendingReq, (int)prv, (long long unsigned)instCount);
        fflush(log_fp);
    }

    virtual void commitUserInstStuck(const uint8_t core, const uint64_t pc,
                                     const uint8_t iType, const int isException,
                                     const int isInterrupt, const uint8_t trapVal,
                                     const uint8_t state, const int claimedPhyReg,
                                     const int ldKilled, const int memAccessAtCommit,
                                     const int lsqAtCommitNotified, const int nonMMIOStDone,
                                     const int epochIncremented, const uint32_t specBits,
                                     const int stbEmpty, const int stqEmpty,
                                     const int tlbNoPendingReq, const uint8_t prv,
                                     const uint64_t instCount) {
        if(ignore_user_commit_stucks > 0) {
            ignore_user_commit_stucks--;
            if(ignore_user_commit_stucks == 0) {
                fprintf(stderr, "  [STOP ignoring user commit DEADLOCKs]\n");
            }
            return;
        }
        fprintf(stderr, "  [DEADLOCK commitUserInstStuck] core %d, pc %016llx, iType %d, isException %d, "
                "isInterrupt %d, trapVal %d, state %d, claimedPhyReg %d, ldKilled %d, "
                "memAccessAtCommit %d, lsqAtCommitNotified %d, nonMMIOStDone %d, epochIncremented %d, "
                "specBits %08x, stbEmpty %d, stqEmpty %d, tlbNoPendingReq %d, prv %d, instCount %llu\n",
                (int)core, (long long unsigned)pc, (int)iType, isException, isInterrupt, (int)trapVal,
                (int)state, claimedPhyReg, ldKilled, memAccessAtCommit, lsqAtCommitNotified,
                nonMMIOStDone, epochIncremented, (unsigned)specBits, stbEmpty, stqEmpty,
                tlbNoPendingReq, (int)prv, (long long unsigned)instCount);
        fprintf(log_fp, "  [DEADLOCK commitUserInstStuck] core %d, pc %016llx, iType %d, isException %d, "
                "isInterrupt %d, trapVal %d, state %d, claimedPhyReg %d, ldKilled %d, "
                "memAccessAtCommit %d, lsqAtCommitNotified %d, nonMMIOStDone %d, epochIncremented %d, "
                "specBits %08x, stbEmpty %d, stqEmpty %d, tlbNoPendingReq %d, prv %d, instCount %llu\n",
                (int)core, (long long unsigned)pc, (int)iType, isException, isInterrupt, (int)trapVal,
                (int)state, claimedPhyReg, ldKilled, memAccessAtCommit, lsqAtCommitNotified,
                nonMMIOStDone, epochIncremented, (unsigned)specBits, stbEmpty, stqEmpty,
                tlbNoPendingReq, (int)prv, (long long unsigned)instCount);
        fflush(log_fp);
    }
};
