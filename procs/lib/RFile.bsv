
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

`include "ProcConfig.bsv"

// Correct use of the register file implies that the same index can't be used for simultaneous read and write from different rules. If different indices are used reads and writes are conflict free. If the reads and writes are in the same rule, write updates the file at the end of the rule.
// We have imitated this conflict free behavior using config regs.
// If we had used ordinary registers, then read<write
// In many designs where we needed Bypass register file, the bypassing was implemented outside the register file, explicitly.


import Types::*;
import ProcTypes::*;
import Vector::*;
import Ehr::*;
import ConfigReg::*;

interface RFile;
    method Action wr( ArchRIndx rindx, Data data );
    method Data rd1( ArchRIndx rindx );
    method Data rd2( ArchRIndx rindx );
    method Data rd3( ArchRIndx rindx );
endinterface

// This is a merged GPR/FPU register file
(* synthesize *)
module mkRFile( RFile );
    let verbose = False;

    Vector#(32, Reg#(Data)) gpr_rfile <- replicateM(mkConfigReg(0));
    Vector#(32, Reg#(Data)) fpu_rfile <- replicateM(mkConfigReg(0));

    function Data read(ArchRIndx rindx);
        return (case (rindx) matches
          tagged Gpr .i: gpr_rfile[i];
          tagged Fpu .i: fpu_rfile[i];
          default: 0;
        endcase);
    endfunction
   
    method Action wr( ArchRIndx rindx, Data data );
        if (verbose) $display(fshow(rindx), " <= %h", data);
        case (rindx) matches
          tagged Gpr .i:
          begin
            if(i!=0) gpr_rfile[i] <= data;
          end
          tagged Fpu .i:
          begin
            fpu_rfile[i] <= data;
          end
        endcase
    endmethod

    method Data rd1( ArchRIndx rindx ) = read(rindx);
    method Data rd2( ArchRIndx rindx ) = read(rindx);
    method Data rd3( ArchRIndx rindx ) = read(rindx);
endmodule 

