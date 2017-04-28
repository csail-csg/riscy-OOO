
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

// Searchable FIFO has an extra search method
interface SMap#(numeric type n, type t, type st);
  method Bool notFull;
  method Action enq(t x);
  method Bool notEmpty;
  method Action deq;
  method Maybe#(t) search(st s);
  method t first;
  method Action clear;
endinterface

// search is conflict-free with {enq, deq, first, notFull, notEmpty}
// search <  clear < canonicalize
module mkCFSMap#(function Bool isFound(t v, st k))(SMap#(n, t, st)) provisos(Bits#(t, tSz), Add#(n, 1, n1), Log#(n1, sz), Add#(sz, 1, sz1));
  Integer ni = valueOf(n);
  Bit#(sz1) nb = fromInteger(ni);
  Bit#(sz1) n2 = 2*nb;
  Vector#(n, Reg#(t)) data <- replicateM(mkRegU);
  Ehr#(3, Bit#(sz1)) enqP <- mkEhr(0);
  Ehr#(3, Bit#(sz1)) deqP <- mkEhr(0);
  Ehr#(3, Bool) enqEn <- mkEhr(True);
  Ehr#(3, Bool) deqEn <- mkEhr(False);
  Ehr#(2, t)                 tempData <- mkEhr(?);
  Ehr#(2, Maybe#(Bit#(sz1))) tempEnqP <- mkEhr(Invalid);
  Ehr#(2, Maybe#(Bit#(sz1))) tempDeqP <- mkEhr(Invalid);

  Bit#(sz1) cnt0 = enqP[0] >= deqP[0]? enqP[0] - deqP[0]: 
                                 (enqP[0]%nb + nb) - deqP[0]%nb;
  Bit#(sz1) cnt2 = enqP[2] >= deqP[2]? enqP[2] - deqP[2]: 
                                 (enqP[2]%nb + nb) - deqP[2]%nb;
  rule canonicalize;
    if(!enqEn[2] && cnt2 != nb) enqEn[2] <= True;
    if(!deqEn[2] && cnt2 != 0) deqEn[2] <= True;

    if(isValid(tempEnqP[1]))
    begin
      data[validValue(tempEnqP[1])] <= tempData[1];
      tempEnqP[1] <= Invalid;
    end

    if(isValid(tempDeqP[1]))
    begin
      deqP[0] <= validValue(tempDeqP[1]);
      tempDeqP[1] <= Invalid;
    end
  endrule

  method Bool notFull = enqEn[0];

  method Action enq(t x) if(enqEn[0]);
    tempData[0] <= x;
    tempEnqP[0] <= Valid (enqP[0]%nb);
    enqP[0] <= (enqP[0] + 1)%n2;
    enqEn[0] <= False;
  endmethod

  method Bool notEmpty = deqEn[0];

  method Action deq if(deqEn[0]);
    tempDeqP[0] <= Valid ((deqP[0] + 1)%n2);
    deqEn[0] <= False;
  endmethod

  method t first if(deqEn[0]);
    return data[deqP[0]%nb];
  endmethod

  method Maybe#(t) search(st s);
    Maybe#(t) ret = Invalid;
    for(Bit#(sz1) i = 0; i < nb; i = i + 1)
    begin
      let ptr = (deqP[0] + i)%nb;
      if(isFound(data[ptr], s) && i < cnt0)
        ret = Valid(data[ptr]);
    end
    return ret;
  endmethod

  method Action clear;
    enqP[1] <= 0;
    deqP[1] <= 0;
    enqEn[1] <= True;
    deqEn[1] <= False;
  endmethod
endmodule

// {notEmpty, first} < deq < search
// search CF {enq, notFull}
// search < clear
module mkPipelineSMap#(function Bool isFound(t v, st k))(SMap#(n, t, st)) provisos(Bits#(t, tSz), Add#(n, 1, n1), Log#(n1, sz), Add#(sz, 1, sz1), Bits#(st, stz));
  Integer ni = valueOf(n);
  Bit#(sz1) nb = fromInteger(ni);
  Bit#(sz1) n2 = 2*nb;
  Vector#(n, Reg#(t)) data <- replicateM(mkRegU);
  Ehr#(3, Bit#(sz1)) enqP <- mkEhr(0);
  Ehr#(2, Bit#(sz1)) deqP <- mkEhr(0);

  Bit#(sz1) cnt0 = enqP[0] >= deqP[0]? enqP[0] - deqP[0]:
                                       (enqP[0]%nb + nb) - deqP[0]%nb;
  Bit#(sz1) cnt1 = enqP[0] >= deqP[1]? enqP[0] - deqP[1]:
                                       (enqP[0]%nb + nb) - deqP[1]%nb;

  method Bool notFull = cnt1 < nb;

  method Action enq(t x) if(cnt1 < nb);
    enqP[0] <= (enqP[0] + 1)%n2;
    data[enqP[0]%nb] <= x;
  endmethod

  method Bool notEmpty = cnt0 != 0;

  method Action deq if(cnt0 != 0);
    deqP[0] <= (deqP[0] + 1)%n2;
  endmethod

  method t first if(cnt0 != 0);
    return data[deqP[0]%nb];
  endmethod

  method Maybe#(t) search(st s);
    Maybe#(t) ret = Invalid;
    for(Bit#(sz1) i = 0; i < nb; i = i + 1)
    begin
      let ptr = (deqP[1] + i)%nb;
      if(isFound(data[ptr], s) && i < cnt1)
        ret = Valid(data[ptr]);
    end
    return ret;
  endmethod

  method Action clear;
    enqP[2] <= 0;
    deqP[1] <= 0;
  endmethod
endmodule

// A Bypass implementation of n element FIFO
// notFull < enq < {notEmpty, first} < deq < clear

module mkBypassSMap#(function Bool isFound(t v, st k))(SMap#(n, t, st)) provisos(Bits#(t, tSz), Add#(n, 1, n1), Log#(n1, sz), Add#(sz, 1, sz1), Bits#(st, stz));
  Integer ni = valueOf(n);
  Bit#(sz1) nb = fromInteger(ni);
  Bit#(sz1) n2 = 2*nb;
  Vector#(n, Ehr#(2, t)) data <- replicateM(mkEhr(?));
  Ehr#(2, Bit#(sz1)) enqP <- mkEhr(0);
  Ehr#(2, Bit#(sz1)) deqP <- mkEhr(0);

  Bit#(sz1) cnt0 = enqP[0] >= deqP[0]? enqP[0] - deqP[0]:
                                       (enqP[0]%nb + nb) - deqP[0]%nb;
  Bit#(sz1) cnt1 = enqP[1] >= deqP[0]? enqP[1] - deqP[0]:
                                       (enqP[1]%nb + nb) - deqP[0]%nb;

  method Bool notFull = cnt0 < nb;

  method Action enq(t x) if(cnt0 < nb);
    enqP[0] <= (enqP[0] + 1)%n2;
    data[enqP[0]%nb][0] <= x;
  endmethod

  method Bool notEmpty = cnt1 != 0;

  method Action deq if(cnt1 != 0);
    deqP[0] <= (deqP[0] + 1)%n2;
  endmethod

  method t first if(cnt1 != 0);
    return data[deqP[0]%nb][1];
  endmethod

  method Maybe#(t) search(st s);
    Maybe#(t) ret = Invalid;
    for(Bit#(sz1) i = 0; i < nb; i = i + 1)
    begin
      let ptr = (deqP[1] + i)%nb;
      if(isFound(data[ptr][1], s) && i < cnt1)
        ret = Valid(data[ptr][1]);
    end
    return ret;
  endmethod

  method Action clear;
    enqP[1] <= 0;
    deqP[1] <= 0;
  endmethod
endmodule
