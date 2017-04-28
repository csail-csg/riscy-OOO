
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

import GetPut::*;
import Connectable::*;
import Vector::*;
import Clocks::*;
import FIFO::*;
import SyncFifo::*;

import DeadlockIF::*;
import Types::*;
import ProcTypes::*;
import MemoryTypes::*;
import CCTypes::*;
import L1CRqMshr::*;
import L1PRqMshr::*;
import L1Bank::*;
import ICRqMshr::*;
import IPRqMshr::*;
import IBank::*;
import L1CoCache::*;
import LLCRqMshr::*;
import LLBank::*;
import LLCDmaConnect::*;
import LLCache::*;
import FetchStage::*;
import EpochManager::*;
import Core::*;
import RenameStage::*;
import CommitStage::*;

interface DeadlockIndInv;
    interface Get#(LLCStuck) llcCRqStuck;
    interface Get#(Tuple2#(CoreId, L1CRqStuck)) dCacheCRqStuck;
    interface Get#(Tuple2#(CoreId, L1PRqStuck)) dCachePRqStuck;
    interface Get#(Tuple2#(CoreId, ICRqStuck)) iCacheCRqStuck;
    interface Get#(Tuple2#(CoreId, IPRqStuck)) iCachePRqStuck;
    interface Get#(Tuple2#(CoreId, RenameStuck)) renameInstStuck;
    interface Get#(Tuple2#(CoreId, RenameStuck)) renameCorrectPathStuck;
    interface Get#(Tuple2#(CoreId, CommitStuck)) commitInstStuck;
    interface Get#(Tuple2#(CoreId, CommitStuck)) commitUserInstStuck;
    interface Get#(CoreId) checkStarted;
endinterface

instance Connectable#(DeadlockIndInv, DeadlockIndication);
    module mkConnection#(DeadlockIndInv inv, DeadlockIndication ind)(Empty);
        rule doDCacheCRqStuck;
            let {c, s} <- inv.dCacheCRqStuck.get;
            ind.dCacheCRqStuck(zeroExtend(c), s.addr, pack(s.op), pack(s.state), pack(s.slotCs), s.waitP);
        endrule

        rule doDCachePRqStuck;
            let {c, s} <- inv.dCachePRqStuck.get;
            ind.dCachePRqStuck(zeroExtend(c), s.addr, pack(s.toState), pack(s.state));
        endrule

        rule doICacheCRqStuck;
            let {c, s} <- inv.iCacheCRqStuck.get;
            ind.iCacheCRqStuck(zeroExtend(c), s.addr, pack(s.state), s.waitP);
        endrule

        rule doICachePRqStuck;
            let {c, s} <- inv.iCachePRqStuck.get;
            ind.iCachePRqStuck(zeroExtend(c), s.addr, pack(s.toState), pack(s.state));
        endrule

        rule doLLCCRqStuck;
            let s <- inv.llcCRqStuck.get;
            LLCStuckSrc src = (case(s.id) matches
                tagged Child ._i: return CoherentChild;
                tagged Dma .dmaId: return (case(dmaId) matches
                    tagged Host ._i: return HostDma;
                    tagged Tlb ._i: return TlbDma;
                endcase);
            endcase);
            function Bit#(2) getDirPendTag(DirPend d);
                return (case(d) matches
                    tagged Invalid: return 0;
                    tagged ToSend ._s: return 1;
                    tagged Waiting ._s: return 2;
                endcase);
            endfunction
            function Bit#(2) getDirPendVal(DirPend d);
                return pack(case(d) matches
                    tagged ToSend .s: return s;
                    tagged Waiting .s: return s;
                    default: return M;
                endcase);
            endfunction
            ind.llcCRqStuck(
                src, zeroExtend(s.child), s.addr, pack(s.fromState),
                pack(s.toState), pack(s.state), s.waitP,
                zeroExtend(pack(map(getDirPendTag, s.dirPend))),
                zeroExtend(pack(map(getDirPendVal, s.dirPend)))
            );
        endrule

        rule doRenameInstStuck;
            let {c, s} <- inv.renameInstStuck.get;
            ind.renameInstStuck(
                zeroExtend(c), s.fetch.pc, zeroExtend(s.fetch.mainEp),
                s.fetch.waitForRedirect, s.fetch.waitForFlush,
                zeroExtend(s.epoch.curEp), zeroExtend(s.epoch.checkedEp),
                s.epoch.waitRedirect, s.htifStall
            );
        endrule

        rule doRenameCorrectPathStuck;
            let {c, s} <- inv.renameCorrectPathStuck.get;
            ind.renameCorrectPathStuck(
                zeroExtend(c), s.fetch.pc, zeroExtend(s.fetch.mainEp),
                s.fetch.waitForRedirect, s.fetch.waitForFlush,
                zeroExtend(s.epoch.curEp), zeroExtend(s.epoch.checkedEp),
                s.epoch.waitRedirect, s.htifStall
            );
        endrule

        function Tuple3#(Bool, Bool, Bit#(4)) decomposeTrap(Maybe#(Trap) trap);
            Bool isException = False;
            Bool isInterrupt = False;
            Bit#(4) trapVal = maxBound;
            if(trap matches tagged Valid .t) begin
                case(t) matches
                    tagged Exception .e: begin
                        isException = True;
                        trapVal = pack(e);
                    end
                    tagged Interrupt .i: begin
                        isInterrupt = True;
                        trapVal = pack(i);
                    end
                endcase
            end
            return tuple3(isException, isInterrupt, trapVal);
        endfunction

        rule doCommitInstStuck;
            let {c, s} <- inv.commitInstStuck.get;
            let {isException, isInterrupt, trapVal} = decomposeTrap(s.trap);
            ind.commitInstStuck(
                zeroExtend(c), s.pc, pack(s.iType), isException, isInterrupt, trapVal,
                pack(s.state), zeroExtend(s.specBits), isValid(s.specTag),
                zeroExtend(fromMaybe(maxBound, s.specTag)), s.stbEmpty, s.prv, s.htifStall
            );
        endrule

        rule doCommitUserInstStuck;
            let {c, s} <- inv.commitUserInstStuck.get;
            let {isException, isInterrupt, trapVal} = decomposeTrap(s.trap);
            ind.commitUserInstStuck(
                zeroExtend(c), s.pc, pack(s.iType), isException, isInterrupt, trapVal,
                pack(s.state), zeroExtend(s.specBits), isValid(s.specTag),
                zeroExtend(fromMaybe(maxBound, s.specTag)), s.stbEmpty, s.prv, s.htifStall
            );
        endrule

        rule doDeadlockCheckStarted;
            let c <- inv.checkStarted.get;
            ind.checkStarted(zeroExtend(c));
        endrule
    endmodule
endinstance

instance Connectable#(DeadlockIndication, DeadlockIndInv);
    module mkConnection#(DeadlockIndication ind, DeadlockIndInv inv)(Empty);
        mkConnection(inv, ind);
    endmodule
endinstance

interface DeadlockSync;
    interface DeadlockRequest req;
    interface DeadlockIndInv indInv;
endinterface

module mkDeadlockSync#(
    Vector#(CoreNum, CoreDeadlock) core, Get#(LLCStuck) llcCRqStuck,
    Clock portalClk, Reset portalRst
)(DeadlockSync);
`ifdef CHECK_DEADLOCK
    Clock userClk <- exposeCurrentClock;
    Reset userRst <- exposeCurrentReset;
    SyncFIFOIfc#(LLCStuck) llcCRqStuckQ <- mkSyncFifo(1, userClk, userRst, portalClk, portalRst);
    SyncFIFOIfc#(Tuple2#(CoreId, L1CRqStuck)) dCacheCRqStuckQ <- mkSyncFifo(1, userClk, userRst, portalClk, portalRst);
    SyncFIFOIfc#(Tuple2#(CoreId, L1PRqStuck)) dCachePRqStuckQ <- mkSyncFifo(1, userClk, userRst, portalClk, portalRst);
    SyncFIFOIfc#(Tuple2#(CoreId, ICRqStuck)) iCacheCRqStuckQ <- mkSyncFifo(1, userClk, userRst, portalClk, portalRst);
    SyncFIFOIfc#(Tuple2#(CoreId, IPRqStuck)) iCachePRqStuckQ <- mkSyncFifo(1, userClk, userRst, portalClk, portalRst);
    SyncFIFOIfc#(Tuple2#(CoreId, RenameStuck)) renameInstStuckQ <- mkSyncFifo(1, userClk, userRst, portalClk, portalRst);
    SyncFIFOIfc#(Tuple2#(CoreId, RenameStuck)) renameCorrectPathStuckQ <- mkSyncFifo(1, userClk, userRst, portalClk, portalRst);
    SyncFIFOIfc#(Tuple2#(CoreId, CommitStuck)) commitInstStuckQ <- mkSyncFifo(1, userClk, userRst, portalClk, portalRst);
    SyncFIFOIfc#(Tuple2#(CoreId, CommitStuck)) commitUserInstStuckQ <- mkSyncFifo(1, userClk, userRst, portalClk, portalRst);
    SyncFIFOIfc#(Tuple2#(CoreId, Bool)) checkStartedQ <- mkSyncFifo(1, userClk, userRst, portalClk, portalRst);

    SyncFIFOIfc#(Data) setCheckStartInstNumQ <- mkSyncFifo(1, portalClk, portalRst, userClk, userRst);

    for(Integer i = 0; i < valueof(CoreNum); i = i+1) begin
        rule sendDCacheCRqStuck;
            let s <- core[i].dCacheCRqStuck.get;
            dCacheCRqStuckQ.enq(tuple2(fromInteger(i), s));
        endrule
        rule sendDCachePRqStuck;
            let s <- core[i].dCachePRqStuck.get;
            dCachePRqStuckQ.enq(tuple2(fromInteger(i), s));
        endrule
        rule sendICacheCRqStuck;
            let s <- core[i].iCacheCRqStuck.get;
            iCacheCRqStuckQ.enq(tuple2(fromInteger(i), s));
        endrule
        rule sendICachePRqStuck;
            let s <- core[i].iCachePRqStuck.get;
            iCachePRqStuckQ.enq(tuple2(fromInteger(i), s));
        endrule
        rule sendRenameInstStuck;
            let s <- core[i].renameInstStuck.get;
            renameInstStuckQ.enq(tuple2(fromInteger(i), s));
        endrule
        rule sendRenameCorrectPathStuck;
            let s <- core[i].renameCorrectPathStuck.get;
            renameCorrectPathStuckQ.enq(tuple2(fromInteger(i), s));
        endrule
        rule sendCommitInstStuck;
            let s <- core[i].commitInstStuck.get;
            commitInstStuckQ.enq(tuple2(fromInteger(i), s));
        endrule
        rule sendCommitUserInstStuck;
            let s <- core[i].commitUserInstStuck.get;
            commitUserInstStuckQ.enq(tuple2(fromInteger(i), s));
        endrule
        rule doCheckStarted;
            let d <- core[i].checkStarted.get;
            checkStartedQ.enq(tuple2(fromInteger(i), ?));
        endrule
    end

    mkConnection(toPut(llcCRqStuckQ), llcCRqStuck);

    rule doSetCheckStartInstNum;
        let n <- toGet(setCheckStartInstNumQ).get;
        for(Integer i = 0; i < valueof(CoreNum); i = i+1) begin
            core[i].setCheckStartInstNum(n);
        end
    endrule

    interface DeadlockRequest req;
        method setCheckStartInstNum = toPut(setCheckStartInstNumQ).put;
    endinterface

    interface DeadlockIndInv indInv;
        interface dCacheCRqStuck = toGet(dCacheCRqStuckQ);
        interface dCachePRqStuck = toGet(dCachePRqStuckQ);
        interface iCacheCRqStuck = toGet(iCacheCRqStuckQ);
        interface iCachePRqStuck = toGet(iCachePRqStuckQ);
        interface llcCRqStuck = toGet(llcCRqStuckQ);
        interface renameInstStuck = toGet(renameInstStuckQ);
        interface renameCorrectPathStuck = toGet(renameCorrectPathStuckQ);
        interface commitInstStuck = toGet(commitInstStuckQ);
        interface commitUserInstStuck = toGet(commitUserInstStuckQ);
        interface Get checkStarted;
            method ActionValue#(CoreId) get;
                let {c, d} <- toGet(checkStartedQ).get;
                return c;
            endmethod
        endinterface
    endinterface

`else

    // FIXME registers in the following two modules are just to make the compiler believe that
    // interface methods are clocked under portal clk (this works with BSC 2015.05)
    module mkNullGet(Get#(t));
        Reg#(Bool) r <- mkRegU;
        method ActionValue#(t) get if(False);
            r <= r;
            return ?;
        endmethod
    endmodule
    module mkNullPut(Put#(t));
        Reg#(Bool) r <- mkRegU;
        method Action put(t x);
            r <= r;
        endmethod
    endmodule

    Get#(Tuple2#(CoreId, L1CRqStuck)) dCacheCRqStuckNull          <- mkNullGet(clocked_by portalClk, reset_by portalRst);
    Get#(Tuple2#(CoreId, L1PRqStuck)) dCachePRqStuckNull          <- mkNullGet(clocked_by portalClk, reset_by portalRst);
    Get#(Tuple2#(CoreId, ICRqStuck)) iCacheCRqStuckNull           <- mkNullGet(clocked_by portalClk, reset_by portalRst);
    Get#(Tuple2#(CoreId, IPRqStuck)) iCachePRqStuckNull           <- mkNullGet(clocked_by portalClk, reset_by portalRst);
    Get#(LLCStuck) llcCRqStuckNull                                <- mkNullGet(clocked_by portalClk, reset_by portalRst);
    Get#(Tuple2#(CoreId, RenameStuck)) renameInstStuckNull        <- mkNullGet(clocked_by portalClk, reset_by portalRst);
    Get#(Tuple2#(CoreId, RenameStuck)) renameCorrectPathStuckNull <- mkNullGet(clocked_by portalClk, reset_by portalRst);
    Get#(Tuple2#(CoreId, CommitStuck)) commitInstStuckNull        <- mkNullGet(clocked_by portalClk, reset_by portalRst);
    Get#(Tuple2#(CoreId, CommitStuck)) commitUserInstStuckNull    <- mkNullGet(clocked_by portalClk, reset_by portalRst);
    Get#(CoreId) checkStartedNull                                 <- mkNullGet(clocked_by portalClk, reset_by portalRst);

    Put#(Data) setCheckStartInstNumNull <- mkNullPut(clocked_by portalClk, reset_by portalRst);

    interface DeadlockRequest req;
        method setCheckStartInstNum = setCheckStartInstNumNull.put;
    endinterface
    
    interface DeadlockIndInv indInv;
        interface dCacheCRqStuck = dCacheCRqStuckNull;
        interface dCachePRqStuck = dCachePRqStuckNull;
        interface iCacheCRqStuck = iCacheCRqStuckNull;
        interface iCachePRqStuck = iCachePRqStuckNull;
        interface llcCRqStuck = llcCRqStuckNull;
        interface renameInstStuck = renameInstStuckNull;
        interface renameCorrectPathStuck = renameCorrectPathStuckNull;
        interface commitInstStuck = commitInstStuckNull;
        interface commitUserInstStuck = commitUserInstStuckNull;
        interface checkStarted = checkStartedNull;
    endinterface
`endif
endmodule
