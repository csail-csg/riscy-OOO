
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


import DDR3Common::*;
import DDR3Controller::*;
import AWSDramCommon::*;
import AWSDramController::*;
import ProcTypes::*;

// VC707 1GB DDR3 instance
typedef DramMaxReads DDR3MaxReadNum;
typedef 10 DDR3SimDelay;

typedef DDR3_1GB_User#(DDR3MaxReadNum, DDR3SimDelay) DDR3UserWrapper;
typedef DDR3_1GB_Full#(DDR3MaxReadNum, DDR3SimDelay) DDR3FullWrapper;

(* synthesize *)
module mkDDR3Wrapper#(Clock sys_clk, Reset sys_rst)(DDR3FullWrapper);
    let m <- mkDDR3_1GB_Controller(sys_clk, sys_rst, True);
    return m;
endmodule

// AWS DRAM instance
typedef DramMaxReads AWSDramMaxReadNum;
typedef DramMaxWrites AWSDramMaxWriteNum;
typedef 10 AWSDramSimDelay;

typedef AWSDramUser#(
    AWSDramMaxReadNum,
    AWSDramMaxWriteNum,
    AWSDramSimDelay
) AWSDramUserWrapper;

typedef AWSDramFull#(
    AWSDramMaxReadNum,
    AWSDramMaxWriteNum,
    AWSDramSimDelay
) AWSDramFullWrapper;

(* synthesize *)
module mkAWSDramWrapper#(Clock dramAxiClk, Reset dramAxiRst)(AWSDramFullWrapper);
    let m <- mkAWSDramController(dramAxiClk, dramAxiRst);
    return m;
endmodule
