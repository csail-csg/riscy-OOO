
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
import Types::*;
import ProcTypes::*;
import Fifo::*;
import FIFO::*;
import XilinxIntMul::*;
import XilinxIntDiv::*;

export SeqMulDivExec(..);
export mkSeqMulDivExec;

function Bool isMulFunc(MulDivFunc func);
    return (case(func)
        Mul, Mulh: True;
        default: False;
    endcase);
endfunction

function XilinxIntMulSign getXilinxMulSign(MulDivSign s);
    return (case(s)
        Signed: (Signed);
        Unsigned: (Unsigned);
        default: (SignedUnsigned);
    endcase);
endfunction

function Bool isDivSigned(MulDivSign s);
    return s == Signed; // there is no SignedUnsigned for div
endfunction

// mul/div that maintains sequential order
interface SeqMulDivExec;
    method Action exec(MulDivInst mdInst, Data rVal1, Data rVal2);
    // output
    method Data   result_data;
    method Action result_deq;
endinterface

(* synthesize *)
module mkSeqMulDivExec(SeqMulDivExec);
    Bool verbose = False;

    XilinxIntMul#(void) mulUnit <- mkXilinxIntMul;
    XilinxIntDiv#(void) divUnit <- mkXilinxIntDiv;

    // This fifo holds what operation is being done by the unit.
    FIFO#(MulDivInst) funcQ <- mkSizedFIFO(`MULDIV_SKID_FIFO_SIZE);

    method Action exec(MulDivInst mdInst, Data rVal1, Data rVal2);
        if(verbose) begin
            $display("[MulDiv] ", fshow(mdInst), ", ",
                     fshow(rVal1), ", ", fshow(rVal2));
        end

        // get the operands, we need to extend correctly in case of OP32. This
        // does not matter for MULW but matters for DIVW.
        Data a = rVal1;
        Data b = rVal2;
        if(mdInst.w) begin
            a = mdInst.sign == Unsigned ? zeroExtend(rVal1[31:0]) :
                                          signExtend(rVal1[31:0]);
            b = mdInst.sign == Signed ? signExtend(rVal2[31:0]) :
                                        zeroExtend(rVal2[31:0]);
        end

        // issue to func unit
        if(isMulFunc(mdInst.func)) begin
            mulUnit.req(a, b, getXilinxMulSign(mdInst.sign), ?);
        end
        else begin
            divUnit.req(a, b, isDivSigned(mdInst.sign), ?);
        end

        // save in bookkeeping fifo
        funcQ.enq(mdInst);
    endmethod

    // output
    method Data result_data;
        let mdInst = funcQ.first;
        Data data = (case(mdInst.func)
            Mul  : (truncate(mulUnit.product));
            Mulh : (truncateLSB(mulUnit.product));
            Div  : (divUnit.quotient);
            Rem  : (divUnit.remainder);
        endcase);
        // correct for OP32 instructions
        if (mdInst.w) begin
            data = signExtend(data[31:0]);
        end
        return data;
    endmethod

    method Action result_deq;
        funcQ.deq;
        let mdInst = funcQ.first;
        if(isMulFunc(mdInst.func)) begin
            mulUnit.deqResp;
        end
        else begin
            divUnit.deqResp;
        end
    endmethod
endmodule

