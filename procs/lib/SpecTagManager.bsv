import Vector::*;
import ProcTypes::*;
import HasSpecBits::*;
import Ehr::*;

interface SpecTagManager;
    method SpecBits currentSpecBits;
    method SpecTag  nextSpecTag;
    method Action   claimSpecTag;
    method Bool     canClaim;
    interface SpeculationUpdate specUpdate;
endinterface

(* synthesize *)
module mkSpecTagManager(SpecTagManager);
    Ehr#(2,SpecBits) current_spec_bits_ehr <- mkEhr(0);
    // normal processing & wrong spec use port 0
    Reg#(SpecBits) current_spec_bits = current_spec_bits_ehr[0];
    // correct spec use port 1

    // dependent_chekcpoints[i] is the SpecBits that depend on SpecTag i.
    // i.e., if SpecTag i is incorrect, then dependent_checkpoints[i] are all
    // wrong.
    Vector#(NumSpecTags, Reg#(SpecBits)) dependent_checkpoints <- replicateM(mkReg(0));

    // wrong spec conflict with claim spec tag
    RWire#(void) wrongSpec_claim_conflict <- mkRWire;

    Maybe#(SpecTag) next_spec_tag = tagged Invalid;
    for (Integer i = valueOf(NumSpecTags) - 1 ; i >= 0 ; i = i-1) begin
        if (current_spec_bits[i] == 0) begin
            next_spec_tag = tagged Valid fromInteger(i);
        end
    end

    rule debugSt;
        if ((next_spec_tag == tagged Invalid )) begin 
            $fdisplay(stdout, "SpecTag manager locked");
        end
    endrule

    method SpecBits currentSpecBits;
        return current_spec_bits;
    endmethod
    method SpecTag nextSpecTag if (next_spec_tag matches tagged Valid .valid_spec_tag);
        return valid_spec_tag;
    endmethod
    method Action claimSpecTag if (next_spec_tag matches tagged Valid .valid_spec_tag);
        current_spec_bits[valid_spec_tag] <= 1;

        for (Integer i = 0 ; i < valueOf(NumSpecTags) ; i = i+1) begin
            if (fromInteger(i) == valid_spec_tag) begin
                dependent_checkpoints[valid_spec_tag] <= (1 << valid_spec_tag);
            end else if (current_spec_bits[i] == 1) begin
                dependent_checkpoints[i] <= dependent_checkpoints[i] | (1 << valid_spec_tag);
            end
        end
        // conflict with wrong spec
        wrongSpec_claim_conflict.wset(?);
    endmethod
    method Bool canClaim = isValid(next_spec_tag);
    interface SpeculationUpdate specUpdate;
        method Action incorrectSpeculation(SpecTag tag);
            current_spec_bits <= current_spec_bits & (~(dependent_checkpoints[tag]));
            // conflict with claim spec tag
            wrongSpec_claim_conflict.wset(?);
        endmethod
        method Action correctSpeculation(SpecBits mask);
            current_spec_bits_ehr[1] <= current_spec_bits_ehr[1] & mask;
        endmethod
    endinterface
endmodule
