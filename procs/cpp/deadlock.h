#pragma once
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include "GeneratedTypes.h"
#include "DeadlockIndication.h"

class DeadlockIndication : public DeadlockIndicationWrapper {
private:
    FILE *log_fp;

public:
    DeadlockIndication(int id) : DeadlockIndicationWrapper(id), log_fp(0) {
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
                                 const uint8_t emCurEp, const uint8_t emCheckedEp,
                                 const int emWaitRedirect, const int htifStall) {
        fprintf(stderr, "  [DEADLOCK renameInstStuck] core %d, pc %016llx, fetchMainEp %d, "
                "fetchWaitRedirect %d, fetchWaitFlush %d, "
                "emCurEp %d, emCheckedEp %d, emWaitRedirect %d, htifStall %d\n",
                (int)core, (long long unsigned)pc, (int)fetchMainEp,
                fetchWaitRedirect, fetchWaitFlush, (int)emCurEp,
                (int)emCheckedEp, emWaitRedirect, htifStall);
        fprintf(log_fp, "[DEADLOCK renameInstStuck] core %d, pc %016llx, fetchMainEp %d, "
                "fetchWaitRedirect %d, fetchWaitFlush %d, "
                "emCurEp %d, emCheckedEp %d, emWaitRedirect %d, htifStall %d\n",
                (int)core, (long long unsigned)pc, (int)fetchMainEp,
                fetchWaitRedirect, fetchWaitFlush, (int)emCurEp,
                (int)emCheckedEp, emWaitRedirect, htifStall);
        fflush(log_fp);
    }

    virtual void renameCorrectPathStuck(uint8_t core, const uint64_t pc, const uint8_t fetchMainEp,
                                        const int fetchWaitRedirect, const int fetchWaitFlush,
                                        const uint8_t emCurEp, const uint8_t emCheckedEp,
                                        const int emWaitRedirect, const int htifStall) {
        fprintf(stderr, "  [DEADLOCK renameCorrectPathStuck] core %d, pc %016llx, fetchMainEp %d, "
                "fetchWaitRedirect %d, fetchWaitFlush %d, "
                "emCurEp %d, emCheckedEp %d, emWaitRedirect %d, htifStall %d\n",
                (int)core, (long long unsigned)pc, (int)fetchMainEp,
                fetchWaitRedirect, fetchWaitFlush, (int)emCurEp,
                (int)emCheckedEp, emWaitRedirect, htifStall);
        fprintf(log_fp, "[DEADLOCK renameCorrectPathStuck] core %d, pc %016llx, fetchMainEp %d, "
                "fetchWaitRedirect %d, fetchWaitFlush %d, "
                "emCurEp %d, emCheckedEp %d, emWaitRedirect %d, htifStall %d\n",
                (int)core, (long long unsigned)pc, (int)fetchMainEp,
                fetchWaitRedirect, fetchWaitFlush, (int)emCurEp,
                (int)emCheckedEp, emWaitRedirect, htifStall);
        fflush(log_fp);
    }

    virtual void commitInstStuck(uint8_t core, const uint64_t pc,
                                 const uint8_t iType, const int isException,
                                 const int isInterrupt, const uint8_t trapVal,
                                 const uint8_t state, const uint16_t specBits,
                                 const int specTagValid, const uint8_t specTag,
                                 const int stbEmpty, const uint8_t prv, const int htifStall) {
        fprintf(stderr, "  [DEADLOCK commitInstStuck] core %d, pc %016llx, iType %d, isException %d, "
                "isInterrupt %d, trapVal %d, state %d, specBits %04x, specTagValid %d, specTag %d, "
                "stbEmpty %d, prv %d, htifStall %d\n",
                (int)core, (long long unsigned)pc, (int)iType, isException, isInterrupt, (int)trapVal,
                (int)state, (unsigned)specBits, specTagValid, (int)specTag, stbEmpty, (int)prv, htifStall);
        fprintf(log_fp, "[DEADLOCK commitInstStuck] core %d, pc %016llx, iType %d, isException %d, "
                "isInterrupt %d, trapVal %d, state %d, specBits %04x, specTagValid %d, specTag %d, "
                "stbEmpty %d, prv %d, htifStall %d\n",
                (int)core, (long long unsigned)pc, (int)iType, isException, isInterrupt, (int)trapVal,
                (int)state, (unsigned)specBits, specTagValid, (int)specTag, stbEmpty, (int)prv, htifStall);
        fflush(log_fp);
    }

    virtual void commitUserInstStuck(uint8_t core, const uint64_t pc,
                                     const uint8_t iType, const int isException,
                                     const int isInterrupt, const uint8_t trapVal,
                                     const uint8_t state, const uint16_t specBits,
                                     const int specTagValid, const uint8_t specTag,
                                     const int stbEmpty, const uint8_t prv, const int htifStall) {
        fprintf(stderr, "  [DEADLOCK commitUserInstStuck] core %d, pc %016llx, iType %d, isException %d, "
                "isInterrupt %d, trapVal %d, state %d, specBits %04x, specTagValid %d, specTag %d, "
                "stbEmpty %d, prv %d, htifStall %d\n",
                (int)core, (long long unsigned)pc, (int)iType, isException, isInterrupt, (int)trapVal,
                (int)state, (unsigned)specBits, specTagValid, (int)specTag, stbEmpty, (int)prv, htifStall);
        fprintf(log_fp, "[DEADLOCK commitUserInstStuck] core %d, pc %016llx, iType %d, isException %d, "
                "isInterrupt %d, trapVal %d, state %d, specBits %04x, specTagValid %d, specTag %d, "
                "stbEmpty %d, prv %d, htifStall %d\n",
                (int)core, (long long unsigned)pc, (int)iType, isException, isInterrupt, (int)trapVal,
                (int)state, (unsigned)specBits, specTagValid, (int)specTag, stbEmpty, (int)prv, htifStall);
        fflush(log_fp);
    }
};
