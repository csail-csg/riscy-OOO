import ProcTypes::*;
import HasSpecBits::*;
import Vector::*;

interface SpecBitsStack#(numeric type size, type t);
    method Action push(t x);

    method t pop_data;
    method Action pop;

    method t commit_data;
    method Action commit;

    interface SpeculationUpdate specUpdate;
endinterface

module mkSpecBitsStack(SpecBitsStack#(size, t)) provisos (Bits#(t, tSz), HasSpecBits#(t));
    // HasSpecBits defines the following:
    // getSpecBits(x);
    // setSpecBits(x, tag);

    Vector#(size, Reg#(t)) row <- replicateM(mkReg(unpack(0)));
    Vector#(size, Reg#(Bool)) valid <- replicateM(mkReg(False));

    Reg#(Bit#(TLog#(size))) enqP <- mkReg(0);
    Reg#(Bit#(TLog#(size))) deqP <- mkReg(0);
    Bit#(TLog#(size)) next_enqP = enqP == fromInteger(valueOf(size)-1) ? 0 : enqP + 1;
    Bit#(TLog#(size)) prev_enqP = enqP == 0 ? fromInteger(valueOf(size)-1) : enqP - 1;
    Bit#(TLog#(size)) next_deqP = deqP == fromInteger(valueOf(size)-1) ? 0 : deqP + 1;
    Bool empty = all( \== (False),  readVReg(valid) );

    rule deqKilledEntry (!valid[deqP] && (enqP != deqP || !empty));
        deqP <= next_deqP;
    endrule

    rule popKilledEntry (!valid[prev_enqP] && (enqP != deqP || !empty));
        deqP <= next_deqP;
    endrule

    method Action push(t x) if (!valid[enqP] && (empty || (enqP != deqP)));
        valid[enqP] <= True;
        row[enqP] <= x;
        enqP <= next_enqP;
    endmethod

    method t pop_data if (valid[prev_enqP]);
        return row[prev_enqP];
    endmethod
    method Action pop if (valid[prev_enqP]);
        valid[prev_enqP] <= False;
        enqP <= prev_enqP;
    endmethod

    method t commit_data if (valid[deqP]);
        return row[deqP];
    endmethod
    method Action commit if (valid[deqP]);
        valid[deqP] <= False;
        deqP <= next_deqP;
    endmethod

    interface SpeculationUpdate specUpdate;
        method Action incorrectSpeculation(SpecTag x);
            for (Integer i = 0 ; i < valueOf(size) ; i = i+1) begin
                if (getSpecBits(row[i])[x] == 1) begin
                    valid[i] <= False;
                end
            end
        endmethod
        method Action correctSpeculation(SpecTag x);
            for (Integer i = 0 ; i < valueOf(size) ; i = i+1) begin
                let new_spec_bits = getSpecBits(row[i]);
                new_spec_bits[x] = 0;
                row[i] <= setSpecBits(row[i], new_spec_bits);
            end
        endmethod
    endinterface
endmodule

