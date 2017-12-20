
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


// currently each DMA access is blocking, so there is no tag in req/resp

// DMA ifc is similar to AXI, addr should be aligned w.r.t dword

typedef enum {HostDmaRead, HostDmaWrite} HostDmaCmd deriving(Bits, Eq, FShow);

typedef 8 LogMaxHostBurstLen;

interface HostDmaRequest;
    method Action req(HostDmaCmd cmd, Bit#(32) addr, Bit#(LogMaxHostBurstLen) burstLenMinusOne);
    method Action wrData(Bit#(64) data, Bit#(8) byteEn, Bool last);
endinterface

interface HostDmaIndication;
    method Action rdData(Bit#(64) data, Bit#(LogMaxHostBurstLen) id);
    method Action wrDone;
    // FPGA DRAM status
    method Action dramErr(Bit#(8) err);
endinterface
