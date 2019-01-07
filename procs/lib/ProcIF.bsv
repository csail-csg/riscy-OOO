
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

import Performance::*;

interface ProcRequest;
    method Action start(
        Bit#(64) startpc,
        Bit#(64) toHostAddr, Bit#(64) fromHostAddr,
        Bit#(64) addrOverflowMask
    );
    method Action from_host(Bit#(64) v);
    method Action perfReq(Bit#(8) core, PerfLocation loc, PerfType t); // performance
endinterface

interface ProcIndication;
    method Action to_host(Bit#(64) v);
    method Action perfResp(Bit#(8) core, ProcPerfResp r); // performance
    method Action terminate(Bit#(8) core); // exit signal
endinterface
