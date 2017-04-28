
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
import HList::*; // for Gettable typeclass
import Vector::*;

typeclass HasSpecBits#(type t);
    function SpecBits getSpecBits(t x);
    function t setSpecBits(t x, SpecBits spec_bits);
endtypeclass

instance HasSpecBits#(SpecBits);
    function SpecBits getSpecBits(SpecBits x) = x;
    function SpecBits setSpecBits(SpecBits x, SpecBits spec_bits) = spec_bits;
endinstance

instance HasSpecBits#(t) provisos (Gettable#(t, SpecBits));
    function SpecBits getSpecBits(t x);
        return getIt(x);
    endfunction
    function t setSpecBits(t x, SpecBits spec_bits);
        return putIt(x, spec_bits);
    endfunction
endinstance

instance HasSpecBits#(Tuple2#(t1, t2)) provisos (HasSpecBits#(t1), HasSpecBits#(t2));
    function SpecBits getSpecBits(Tuple2#(t1, t2) x);
        return getSpecBits(tpl_1(x)) | getSpecBits(tpl_2(x));
    endfunction
    function Tuple2#(t1, t2) setSpecBits(Tuple2#(t1, t2)  x, SpecBits spec_bits);
        return tuple2(setSpecBits(tpl_1(x), spec_bits), setSpecBits(tpl_2(x), spec_bits));
    endfunction
endinstance

`define HasSpecBitsInstanceForStruct(mystruct) instance HasSpecBits#(mystruct); \
    function SpecBits getSpecBits(mystruct x); \
        return x.spec_bits; \
    endfunction \
    function mystruct setSpecBits(mystruct x, SpecBits spec_bits); \
        x.spec_bits = spec_bits; \
        return x; \
    endfunction \
endinstance

// TODO: Add HasSpecBitsInstanceForStruct(mystruct) for structs that have spec_bits

//---------------------------------------------
// Some modules for types with HasSpecBits#(t)
//---------------------------------------------

// per individual instructions
interface SpeculationStatus;
    method Bool killed;
    method SpecBits specBits;
    method Bool speculative;
    method Bool dependsOn(SpecTag tag);
endinterface

// actions that can be applied to data structures
interface SpeculationUpdate;
    method Action incorrectSpeculation(SpecTag tag);
    method Action correctSpeculation(SpecBits mask); // mask out corrected spec bits, i.e. sb <= sb & mask
endinterface

function SpeculationUpdate joinSpeculationUpdate( Vector#(n, SpeculationUpdate) vec );
    return (interface SpeculationUpdate;
            method Action incorrectSpeculation(SpecTag tag);
                for (Integer i = 0 ; i < valueOf(n) ; i = i+1) begin
                    vec[i].incorrectSpeculation(tag);
                end
            endmethod
            method Action correctSpeculation(SpecBits mask);
                for (Integer i = 0 ; i < valueOf(n) ; i = i+1) begin
                    vec[i].correctSpeculation(mask);
                end
            endmethod
        endinterface);
endfunction

/*
interface SpecReg#(type t);
    method Action _write(t x);
    method t _read;
    interface SpeculationStatus specStatus;
    interface SpeculationUpdate specUpdate;
endinterface

module mkSpecReg#(t init)(SpecReg#(t)) provisos (HasSpecBits#(t), Bits#(t, tSz));
    Reg#(Bool) killed <- mkReg(False);
    Reg#(t) data <- mkReg(init);

    method Action _write(t x);
        data <= x;
        killed <= False;
    endmethod
    method t _read;
        return data;
    endmethod

    interface SpeculationStatus specStatus;
        method Bool killed;
            return killed;
        endmethod
        method SpecBits specBits;
            return getSpecBits(data);
        endmethod
        method Bool speculative;
            return (getSpecBits(data) != 0);
        endmethod
        method Bool dependsOn(SpecTag tag);
            return unpack(getSpecBits(data)[tag]);
        endmethod
    endinterface
    interface SpeculationUpdate specUpdate;
        method Action incorrectSpeculation(SpecTag tag);
            let spec_bits = getSpecBits(data);
            if (spec_bits[tag] == 1) begin
                killed <= True;
                spec_bits[tag] = 0;
                data <= setSpecBits(data, spec_bits);
            end
        endmethod
        method Action correctSpeculation(SpecTag tag);
            let spec_bits = getSpecBits(data);
            if (spec_bits[tag] == 1) begin
                spec_bits[tag] = 0;
                data <= setSpecBits(data, spec_bits);
            end
        endmethod
    endinterface
endmodule

interface SpecCheckpointReg#(type t);
    method Action _write(t x);
    method t _read;
    method Action claimCheckpoint(SpecTag tag, t value);
    interface SpeculationUpdate specUpdate;
endinterface

module mkSpecCheckpointReg#(t init)(SpecCheckpointReg#(t)) provisos (Bits#(t,tSz));
    Reg#(t) main <- mkReg(init);
    Vector#(NumSpecTags, Reg#(t)) checkpoints <- replicateM(mkReg(init));

    method Action _write(t x);
        main <= x;
    endmethod
    method t _read;
        return main;
    endmethod

    method Action claimCheckpoint(SpecTag tag, t value);
        checkpoints[tag] <= value;
    endmethod

    interface SpeculationUpdate specUpdate;
        method Action incorrectSpeculation(SpecTag tag);
            // revert to checkpoint
            main <= checkpoints[tag];
        endmethod
        method Action correctSpeculation(SpecTag tag);
            noAction;
        endmethod
    endinterface
endmodule
*/
