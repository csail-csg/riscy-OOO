
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

import RenameDebugIF::*;
import Types::*;
import ProcTypes::*;
import CommitStage::*;
import Core::*;

interface RenameDebugIndInv;
    interface Get#(Tuple2#(CoreId, RenameErrInfo)) renameErr;
endinterface

instance Connectable#(RenameDebugIndInv, RenameDebugIndication);
    module mkConnection#(RenameDebugIndInv inv, RenameDebugIndication ind)(Empty);
        rule doRenameErr;
            let {c, r} <- inv.renameErr.get;
            // get trap e
            Bool isException = False;
            Bool isInterrupt = False;
            Bit#(4) trapVal = maxBound;
            if(r.trap matches tagged Valid .t) begin
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
            // send to ind
            ind.renameErr(
                zeroExtend(c), r.err, r.pc, pack(r.iType),
                isException, isInterrupt, trapVal,
                zeroExtend(r.specBits)
            );
        endrule
    endmodule
endinstance

instance Connectable#(RenameDebugIndication, RenameDebugIndInv);
    module mkConnection#(RenameDebugIndication ind, RenameDebugIndInv inv)(Empty);
        mkConnection(inv, ind);
    endmodule
endinstance

interface RenameDebugSync;
    interface RenameDebugIndInv indInv;
endinterface

module mkRenameDebugSync#(
    Vector#(CoreNum, CoreRenameDebug) core,
    Clock portalClk, Reset portalRst
)(RenameDebugSync);
    Clock userClk <- exposeCurrentClock;
    Reset userRst <- exposeCurrentReset;
    SyncFIFOIfc#(Tuple2#(CoreId, RenameErrInfo)) renameErrQ <- mkSyncFifo(1, userClk, userRst, portalClk, portalRst);

    for(Integer i = 0; i < valueof(CoreNum); i = i+1) begin
        rule sendRenameErr;
            let e <- core[i].renameErr.get;
            renameErrQ.enq(tuple2(fromInteger(i), e));
        endrule
    end

    interface RenameDebugIndInv indInv;
        interface renameErr = toGet(renameErrQ);
    endinterface
endmodule
