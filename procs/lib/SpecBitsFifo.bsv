
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

import ProcTypes::*;
import HasSpecBits::*;
import Vector::*;
import Ehr::*;

/*
interface SpecBitsFifo#(numeric type size, type t);
    method Action enq(t x);
    method Action deq;
    method t first;
    interface SpeculationUpdate specUpdate;
endinterface

module mkSpecBitsFifo(SpecBitsFifo#(size, t)) provisos (Bits#(t, tSz), HasSpecBits#(t));
    // HasSpecBits defines the following:
    // getSpecBits(x);
    // setSpecBits(x, tag);

    Vector#(size, Reg#(t)) row <- replicateM(mkReg(unpack(0)));
    Vector#(size, Ehr#(3,Bool)) valid <- replicateM(mkEhr(False));
    Reg#(Bit#(TLog#(size))) enqP <- mkReg(0);
    Reg#(Bit#(TLog#(size))) deqP <- mkReg(0);
    Bit#(TLog#(size)) next_enqP = enqP == fromInteger(valueOf(size)-1) ? 0 : enqP + 1;
    Bit#(TLog#(size)) next_deqP = deqP == fromInteger(valueOf(size)-1) ? 0 : deqP + 1;
    Bool empty = all( \== (False),  readVEhr(0,valid) );
    Bool empty_for_canonicalize = all( \== (False),  readVEhr(1,valid) );

    rule canonicalize_deqP (!valid[deqP][1] && (enqP != deqP || !empty_for_canonicalize));
        // element at deqP was killed, so increment deqP
        deqP <= next_deqP;
    endrule

    method Action enq(t x) if (!valid[enqP][0] && (empty || (enqP != deqP)));
        valid[enqP][0] <= True;
        row[enqP] <= x;
        enqP <= next_enqP;
    endmethod
    method Action deq if (valid[deqP][0]);
        valid[deqP][0] <= False;
        deqP <= next_deqP;
    endmethod
    method t first if (valid[deqP][0]);
        return row[deqP];
    endmethod
    interface SpeculationUpdate specUpdate;
        method Action incorrectSpeculation(SpecTag x);
            for (Integer i = 0 ; i < valueOf(size) ; i = i+1) begin
                if (getSpecBits(row[i])[x] == 1) begin
                    valid[i][2] <= False;
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

interface PoisonSpecBitsFifo#(numeric type size, type t);
    method Action enq(t x);
    method Action deq;
    method t first_data;
    method Bool first_poisoned;
    interface SpeculationUpdate specUpdate;
endinterface

module mkPoisonSpecBitsFifo(PoisonSpecBitsFifo#(size, t)) provisos (Bits#(t, tSz), HasSpecBits#(t));
    // HasSpecBits defines the following:
    // getSpecBits(x);
    // setSpecBits(x, tag);

    Vector#(size, Reg#(t)) row <- replicateM(mkReg(unpack(0)));
    Vector#(size, Reg#(Bool)) valid <- replicateM(mkReg(False));
    Vector#(size, Ehr#(2,Bool)) poisoned <- replicateM(mkEhr(False));
    Reg#(Bit#(TLog#(size))) enqP <- mkReg(0);
    Reg#(Bit#(TLog#(size))) deqP <- mkReg(0);
    Bit#(TLog#(size)) next_enqP = enqP == fromInteger(valueOf(size)-1) ? 0 : enqP + 1;
    Bit#(TLog#(size)) next_deqP = deqP == fromInteger(valueOf(size)-1) ? 0 : deqP + 1;

    method Action enq(t x) if (!valid[enqP]);
        valid[enqP] <= True;
        poisoned[enqP][0] <= False;
        row[enqP] <= x;
        enqP <= next_enqP;
    endmethod
    method Action deq if (valid[deqP]);
        valid[deqP] <= False;
        poisoned[deqP][0] <= False;
        deqP <= next_deqP;
    endmethod
    method t first_data if (valid[deqP]);
        return row[deqP];
    endmethod
    method Bool first_poisoned if (valid[deqP]);
        return poisoned[deqP][0];
    endmethod
    interface SpeculationUpdate specUpdate;
        method Action incorrectSpeculation(SpecTag x);
            for (Integer i = 0 ; i < valueOf(size) ; i = i+1) begin
                if (getSpecBits(row[i])[x] == 1) begin
                    poisoned[i][1] <= True;
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

module mkPoisonSpecBitsFifoEhr(PoisonSpecBitsFifo#(size, t)) provisos (Bits#(t, tSz), HasSpecBits#(t));
    // HasSpecBits defines the following:
    // getSpecBits(x);
    // setSpecBits(x, tag);

    Integer enqPort = 1; // doDispatch rule: enq
    Integer deqPort = 0; // doFinish rule: deq
    Integer wrongSpecPort = 0; // incorrect speculation redirection rules are scheduled first, so port 0
    Integer correctSpecPort = 2; // correct spec always at last

    // make incorrectSpeculation conflict with enq
    RWire#(void) enq_incorrectSpeculation_conflict <- mkRWire;

    Vector#(size, Ehr#(3, t)) row <- replicateM(mkEhr(unpack(0)));
    Vector#(size, Ehr#(2, Bool)) valid <- replicateM(mkEhr(False));
    Vector#(size, Ehr#(2, Bool)) poisoned <- replicateM(mkEhr(False));
    Reg#(Bit#(TLog#(size))) enqP <- mkReg(0);
    Reg#(Bit#(TLog#(size))) deqP <- mkReg(0);
    Bit#(TLog#(size)) next_enqP = enqP == fromInteger(valueOf(size)-1) ? 0 : enqP + 1;
    Bit#(TLog#(size)) next_deqP = deqP == fromInteger(valueOf(size)-1) ? 0 : deqP + 1;

    method Action enq(t x) if (!valid[enqP][enqPort]);
        valid[enqP][enqPort] <= True;
        poisoned[enqP][enqPort] <= False;
        row[enqP][enqPort] <= x;
        enqP <= next_enqP;
        // make conflict with incorrectSpeculation
        enq_incorrectSpeculation_conflict.wset(?);
    endmethod

    method Action deq if (valid[deqP][deqPort]);
        valid[deqP][deqPort] <= False;
        // [sizhuo] I don't think we need to reset poisoned bit, because valid bit is reset
        // this allows deq and incorrectSpeculation to fire in the same rule
        //poisoned[deqP][deqPort] <= False;
        deqP <= next_deqP;
    endmethod
    
    method t first_data if (valid[deqP][deqPort]);
        return row[deqP][deqPort];
    endmethod
    
    method Bool first_poisoned if (valid[deqP][deqPort]);
        return poisoned[deqP][deqPort];
    endmethod
    
    interface SpeculationUpdate specUpdate;
        method Action incorrectSpeculation(SpecTag x);
            for (Integer i = 0 ; i < valueOf(size) ; i = i+1) begin
                if (getSpecBits(row[i][wrongSpecPort])[x] == 1) begin
                    poisoned[i][wrongSpecPort] <= True;
                end
            end
            // make conflicts with enq
            enq_incorrectSpeculation_conflict.wset(?);
        endmethod
        method Action correctSpeculation(SpecBits mask);
            for (Integer i = 0 ; i < valueOf(size) ; i = i+1) begin
                let new_spec_bits = getSpecBits(row[i][correctSpecPort]) & mask;
                row[i][correctSpecPort] <= setSpecBits(row[i][correctSpecPort], new_spec_bits);
            end
        endmethod
    endinterface
endmodule
*/
