import ProcTypes::*;
import HasSpecBits::*;
import GetPut::*;
import Vector::*;

interface GSpecUpdate#(numeric type correctSpecPortNum);
    interface Vector#(correctSpecPortNum, Put#(SpecTag)) correctSpec;
    method Action incorrectSpec(SpecTag spec_tag);
endinterface

module mkGSpecUpdate#(SpeculationUpdate ifc)(GSpecUpdate#(correctSpecPortNum));
    // record correct spec tags
    Vector#(correctSpecPortNum, RWire#(SpecTag)) correctSpecTag <- replicateM(mkRWire);
    // make wrong spec conflict with correct spec
    Vector#(correctSpecPortNum, RWire#(void)) spec_conflict <- replicateM(mkRWire);

    (* fire_when_enabled, no_implicit_conditions *)
    rule canon_correct_spec;
        SpecBits mask = maxBound;
        for(Integer i = 0; i < valueof(correctSpecPortNum); i = i+1) begin
            if(correctSpecTag[i].wget matches tagged Valid .tag) begin
                mask[tag] = 0;
            end
        end
        ifc.correctSpeculation(mask);
    endrule

    Vector#(correctSpecPortNum, Put#(SpecTag)) correctVec = ?;
    for(Integer i = 0; i < valueof(correctSpecPortNum); i = i+1) begin
        correctVec[i] = (interface Put;
            method Action put(SpecTag t);
                correctSpecTag[i].wset(t);
                // conflict with wrong spec
                spec_conflict[i].wset(?);
            endmethod
        endinterface);
    end

    interface correctSpec = correctVec;

    method Action incorrectSpec(SpecTag spec_tag);
        ifc.incorrectSpeculation(spec_tag);
        // conflict with correct spec
        for(Integer i = 0; i < valueof(correctSpecPortNum); i = i+1) begin
            spec_conflict[i].wset(?);
        end
    endmethod
endmodule
