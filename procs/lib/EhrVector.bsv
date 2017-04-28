
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

import Ehr::*;
import Vector::*;
import RWire::*;
import RevertingVirtualReg::*;

module mkEhrVector#(t init)(Vector#(elemNum, Ehr#(portNum, t))) provisos (
    Bits#(t, tSz),
    Alias#(elemIdxT, Bit#(TLog#(elemNum)))
);
    Vector#(elemNum, Reg#(t)) data <- replicateM(mkReg(init));

    // record write cmd
    Vector#(portNum, RWire#(Tuple2#(elemIdxT, t))) writeCmd <- replicateM(mkUnsafeRWire);

    // write-write ordering
    Vector#(portNum, Vector#(portNum, RWire#(Bool))) ww <- replicateM(replicateM(mkUnsafeRWire));

    // read-write/write-read ordering
    Vector#(portNum, Reg#(Bool)) rw <- replicateM(mkRevertingVirtualReg(True)); // must be init to True

    for(Integer i = 0; i < valueof(elemNum); i = i+1) begin
        (* fire_when_enabled, no_implicit_conditions *)
        rule canon;
            t next = data[i];
            for(Integer j = 0; j < valueof(portNum); j = j+1) begin
                if(writeCmd[j].wget matches tagged Valid {.idx, .d} &&& idx == fromInteger(i)) begin
                    next = d;
                end
            end
            data[i] <= next;
        endrule
    end

    Vector#(elemNum, Ehr#(portNum, t)) ifc = ?;

    for(Integer i = 0; i < valueof(elemNum); i = i+1) begin
        for(Integer j = 0; j < valueof(portNum); j = j+1) begin
            ifc[i][j] = (interface Reg;
                method Action _write(t x);
                    writeCmd[j].wset(tuple2(fromInteger(i), x));
                    // order after write on smaller ports
                    for(Integer k = 0; k < j; k = k+1) begin
                        ww[j][k].wset(isValid(writeCmd[k].wget));
                    end
                    // order with reads
                    rw[j] <= ?;
                endmethod
                method t _read;
                    t val = data[i];
                    for(Integer k = 0; k < j; k = k+1) begin
                        if(writeCmd[k].wget matches tagged Valid {.idx, .d} &&& idx == fromInteger(i)) begin
                            val = d;
                        end
                    end
                    // order with writes
                    Bool true = True;
                    for(Integer k = j; k < valueof(portNum); k = k+1) begin
                        true = true && rw[k];
                    end
                    return true ? val : unpack(0);
                    // use a non-? val here! otherwise new BSV compiler will stop optimize at ? val
                    // this affects judging if two rules are exclusive
                endmethod
            endinterface);
        end
    end

    return ifc;
endmodule
