
// Copyright (c) 2018 Massachusetts Institute of Technology
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

import Types::*;
import ProcTypes::*;
import Vector::*;

interface ArchRegFile;
    method Data rd(ArchRIndx idx);
    method Action wr(ArchRIndx idx, Data val);
endinterface

module mkArchRegFile(ArchRegFile) provisos(
    NumAlias#(gprNum, TExp#(SizeOf#(GprRIndx))),
    NumAlias#(fpuNum, TExp#(SizeOf#(FpuRIndx)))
);
    Vector#(gprNum, Reg#(Data)) gpr;
    // GPR 0 is always 0
    gpr[0] = (interface Reg;
        method Data _read;
            return 0;
        endmethod
        method Action _write(Data v);
            noAction;
        endmethod
    endinterface);
    for(Integer i = 1; i < valueof(gprNum); i = i + 1) begin
        gpr[i] <- mkReg(0);
    end

    Vector#(fpuNum, Reg#(Data)) fpu <- replicateM(mkReg(0));

    method Data rd(ArchRIndx idx);
        case(idx) matches
            tagged Gpr .i: return gpr[i];
            tagged Fpu .i: return fpu[i];
            default: return 0;
        endcase
    endmethod

    method Action wr(ArchRIndx idx, Data val);
        case(idx) matches
            tagged Gpr .i: gpr[i] <= val;
            tagged Fpu .i: fpu[i] <= val;
            default: noAction;
        endcase
    endmethod
endmodule
