`include "ProcConfig.bsv"
import Vector::*;
import Ehr::*;
import Types::*;
import ProcTypes::*;

typedef struct {
    Epoch curEp;
    Epoch checkedEp;
    Bool waitRedirect;
} EpochDebugState deriving(Bits, Eq, FShow);

interface EM_checkEpoch;
    method Bool check(Epoch e);
endinterface

interface EM_updatePrevEpoch;
    method Action update(Epoch e);
endinterface

interface EpochManager;
    interface Vector#(SupSize, EM_checkEpoch) checkEpoch;
    interface Vector#(SupSize, EM_updatePrevEpoch) updatePrevEpoch;
    method Epoch getEpoch;
    method Action incrementEpochWithoutRedirect;
    method Action redirect;
    // for debug
    method EpochDebugState getEpochState;
endinterface

(* synthesize *)
module mkEpochManager(EpochManager);
    Reg#(Epoch) curr_epoch <- mkReg(0);
    Reg#(Epoch) prev_checked_epoch <- mkReg(0);
    Reg#(Bool) waiting_for_redirect <- mkReg(False);
    Epoch next_epoch = (curr_epoch== fromInteger(valueOf(NumEpochs)-1)) ? 0 : (curr_epoch+1);

    // epochs in the core are within range [prev_checked_epoch, curr_epoch]
    // prev_checked_epoch can be updated in a lazy way
    Vector#(SupSize, Ehr#(2, Maybe#(Epoch))) updatePrevEn <- replicateM(mkEhr(Invalid));

    (* fire_when_enabled, no_implicit_conditions *)
    rule canon_prev_checked_epoch;
        Vector#(SupSize, Maybe#(Epoch)) updates = readVEhr(1, updatePrevEn);
        // find the last update
        if(find(isValid, reverse(updates)) matches tagged Valid .upd) begin
            doAssert(isValid(upd), "must be valid");
            prev_checked_epoch <= validValue(upd);
        end
        // reset EHR
        for(Integer i = 0; i < valueof(SupSize); i = i+1) begin
            updatePrevEn[i][1] <= Invalid;
        end
    endrule

    Vector#(SupSize, EM_updatePrevEpoch) updateIfc;
    for(Integer i = 0; i < valueof(SupSize); i = i+1) begin
        updateIfc[i] = (interface EM_updatePrevEpoch;
            method Action update(Epoch e);
                updatePrevEn[i][0] <= Valid (e); // record update action
`ifdef BSIM
                // sanity check
                Epoch checkedEpoch = prev_checked_epoch;
                for(Integer j = 0; j < i; j = j+1) begin
                    if(updatePrevEn[j][1] matches tagged Valid .ep) begin
                        checkedEpoch = ep;
                    end
                end
                if(checkedEpoch <= curr_epoch) begin
                    doAssert(checkedEpoch <= e && e <= curr_epoch, "e in [checkedEpoch, curr_epoch]");
                end
                else begin
                    doAssert(checkedEpoch <= e || e <= curr_epoch, "e in [checkedEpoch, max] + [0, curr_epoch]");
                end
`endif
            endmethod
        endinterface);
    end

    Vector#(SupSize, EM_checkEpoch) checkIfc;
    for(Integer i = 0; i < valueof(SupSize); i = i+1) begin
        checkIfc[i] = (interface EM_checkEpoch;
            method Bool check(Epoch e);
                return (e == curr_epoch);
            endmethod
        endinterface);
    end

    interface updatePrevEpoch = updateIfc;

    interface checkEpoch = checkIfc;

    method Epoch getEpoch;
        return curr_epoch;
    endmethod
    method Action incrementEpochWithoutRedirect if(prev_checked_epoch != next_epoch);
        // It is actually okay to call this twice without redirecting. This
        // could happen in the case where you have a memory instruction that
        // causees a page fault followed by an eret. The eret will call this
        // method first, then the page fault will call the method again and
        // redirect later.
        if (!waiting_for_redirect) begin
            curr_epoch <= next_epoch;
        end
        waiting_for_redirect <= True;
    endmethod
    method Action redirect if (prev_checked_epoch != next_epoch || waiting_for_redirect);
        if (waiting_for_redirect) begin
            // epoch was flipped previously
            waiting_for_redirect <= False;
        end else begin
            curr_epoch <= next_epoch;
        end
    endmethod

    method EpochDebugState getEpochState;
        return EpochDebugState {
            curEp: curr_epoch,
            checkedEp: prev_checked_epoch,
            waitRedirect: waiting_for_redirect
        };
    endmethod
endmodule

