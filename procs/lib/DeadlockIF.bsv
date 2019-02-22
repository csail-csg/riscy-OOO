
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
        Bit#(8) emCurEp, Bit#(8) emCheckedEp
    );
    method Action renameCorrectPathStuck(
        Bit#(8) core, Bit#(64) pc, Bit#(8) fetchMainEp,
        Bool fetchWaitRedirect, Bool fetchWaitFlush,
        Bit#(8) emCurEp, Bit#(8) emCheckedEp
    );
    method Action commitInstStuck(
        Bit#(8) core, Bit#(64) pc, Bit#(5) iType,
        Bool isException, Bool isInterrupt, Bit#(4) trapVal,
        Bit#(1) state, Bool claimedPhyReg, Bool ldKilled,
        Bool memAccessAtCommit, Bool lsqAtCommitNotified,
        Bool nonMMIOStDone, Bool epochIncremented,
        Bit#(32) specBits, Bool stbEmpty, Bool stqEmpty,
        Bool tlbNoPendingReq, Bit#(2) prv, Bit#(64) instCount
    );
    method Action commitUserInstStuck(
        Bit#(8) core, Bit#(64) pc, Bit#(5) iType,
        Bool isException, Bool isInterrupt, Bit#(4) trapVal,
        Bit#(1) state, Bool claimedPhyReg, Bool ldKilled,
        Bool memAccessAtCommit, Bool lsqAtCommitNotified,
        Bool nonMMIOStDone, Bool epochIncremented,
        Bit#(32) specBits, Bool stbEmpty, Bool stqEmpty,
        Bool tlbNoPendingReq, Bit#(2) prv, Bit#(64) instCount
    );
endinterface
