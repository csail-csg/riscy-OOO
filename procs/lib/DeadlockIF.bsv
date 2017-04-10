
interface DeadlockRequest;
    // deadlock check for core starts after this number of inst
    // deadlock check for caches always starts immediately
    method Action setCheckStartInstNum(Bit#(64) n);
endinterface

typedef enum {
    HostDma,
    TlbDma,
    CoherentChild
} LLCStuckSrc deriving(Bits, Eq, FShow);

interface DeadlockIndication;
    method Action llcCRqStuck(
        LLCStuckSrc src, Bit#(8) childId, Bit#(64) addr,
        Bit#(2) fromState, Bit#(2) toState, Bit#(3) state, Bool waitP,
        Bit#(32) dirPendTag, Bit#(32) dirPendState // at most 16 cores
    );
    method Action dCacheCRqStuck(Bit#(8) core, Bit#(64) addr, Bit#(3) op, Bit#(3) state, Bit#(2) slotCs, Bool waitP);
    method Action dCachePRqStuck(Bit#(8) core, Bit#(64) addr, Bit#(2) toState, Bit#(2) state);
    method Action iCacheCRqStuck(Bit#(8) core, Bit#(64) addr, Bit#(3) state, Bool waitP);
    method Action iCachePRqStuck(Bit#(8) core, Bit#(64) addr, Bit#(2) toState, Bit#(2) state);

    method Action checkStarted(Bit#(8) core);
    method Action renameInstStuck(
        Bit#(8) core, Bit#(64) pc, Bit#(8) fetchMainEp,
        Bool fetchWaitRedirect, Bool fetchWaitFlush,
        Bit#(8) emCurEp, Bit#(8) emCheckedEp, Bool emWaitRedirect,
        Bool htifStall
    );
    method Action renameCorrectPathStuck(
        Bit#(8) core, Bit#(64) pc, Bit#(8) fetchMainEp,
        Bool fetchWaitRedirect, Bool fetchWaitFlush,
        Bit#(8) emCurEp, Bit#(8) emCheckedEp, Bool emWaitRedirect,
        Bool htifStall
    );
    method Action commitInstStuck(
        Bit#(8) core, Bit#(64) pc, Bit#(5) iType,
        Bool isException, Bool isInterrupt, Bit#(4) trapVal,
        Bit#(2) state, Bit#(16) specBits, Bool specTagValid ,Bit#(4) specTag,
        Bool stbEmpty, Bit#(2) prv, Bool htifStall
    );
    method Action commitUserInstStuck(
        Bit#(8) core, Bit#(64) pc, Bit#(5) iType,
        Bool isException, Bool isInterrupt, Bit#(4) trapVal,
        Bit#(2) state, Bit#(16) specBits, Bool specTagValid, Bit#(4) specTag,
        Bool stbEmpty, Bit#(2) prv, Bool htifStall
    );
endinterface
