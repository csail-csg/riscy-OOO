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
                r.specBits, isValid(r.specTag), zeroExtend(fromMaybe(maxBound, r.specTag))
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
