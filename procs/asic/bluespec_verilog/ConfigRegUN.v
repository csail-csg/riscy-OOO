
// Copyright (c) 2000-2009 Bluespec, Inc.

// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:

// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.

// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
// THE SOFTWARE.
//
// $Revision: 24080 $
// $Date: 2011-05-18 19:32:52 +0000 (Wed, 18 May 2011) $

`ifdef BSV_ASSIGNMENT_DELAY
`else
`define BSV_ASSIGNMENT_DELAY
`endif


// Basic register without reset.
module ConfigRegUN(CLK, EN, D_IN, Q_OUT);
   parameter width = 1;

   input     CLK;
   input     EN;
   input [width - 1 : 0] D_IN;

   output [width - 1 : 0] Q_OUT;
   reg [width - 1 : 0]    Q_OUT;

`ifdef BSV_NO_INITIAL_BLOCKS
`else // not BSV_NO_INITIAL_BLOCKS
   // synopsys translate_off
   initial begin
      Q_OUT = {((width + 1)/2){2'b10}} ;
   end
   // synopsys translate_on
`endif // BSV_NO_INITIAL_BLOCKS

   
   always@(posedge CLK)
     begin
        if (EN)
          Q_OUT <= `BSV_ASSIGNMENT_DELAY D_IN;
     end
endmodule

