// Copyright (c) 2013 Bluespec, Inc.  All rights reserved.
// $Revision: 32843 $
// $Date: 2013-12-16 16:25:57 +0000 (Mon, 16 Dec 2013) $

package SquareRoot_Riscy;

import ClientServer ::*;
import FIFO ::*;
import FIFO ::*;
import FIFOF ::*;
import FixedPoint ::*;
import GetPut ::*;
import Real ::*;
import StmtFSM ::*;
import Vector ::*;

// ADDED BY ANDY BECAUSE SYNTHESIS IS HAVING PROBLEMS WITH FIFOL1
import Ehr::*;

export mkSquareRooter;
export mkFixedPointSquareRooter;

// ADDED BY ANDY BECAUSE SYNTHESIS IS HAVING PROBLEMS WITH FIFOL1
module mkLFIFO(FIFO#(t)) provisos (Bits#(t,tSz));
  Ehr#(2, Bool) valid <- mkEhr(False);
  Reg#(t) data <- mkReg(unpack(0));

  method Action enq(t x) if (!valid[1]);
    data <= x;
    valid[1] <= True;
  endmethod
  method Action deq() if (valid[0]);
    valid[0] <= False;
  endmethod
  method t first if (valid[0]);
    return data;
  endmethod
  method Action clear();
    valid[1] <= False;
  endmethod
endmodule

module mkSquareRooter#(Integer n)(Server#(UInt#(m),Tuple2#(UInt#(m),Bool)))
   provisos(
      // per request of bsc
      Add#(a__, 2, m),
      Log#(TAdd#(1, m), TLog#(TAdd#(m, 1)))
      );

   FIFO#(UInt#(m)) fRequest <- mkLFIFO;
   FIFO#(Tuple2#(UInt#(m),Bool)) fResponse <- mkLFIFO;

   FIFO#(Tuple4#(Maybe#(Bit#(m)),Bit#(m),Bit#(m),Bit#(m))) fFirst <- mkLFIFO;

   rule start;
      let op <- toGet(fRequest).get;
      let s = pack(op);
      Bit#(m) r = 0;
      Bit#(m) b = reverseBits(extend(2'b10));

      let s0 = countZerosMSB(s);
      let b0 = countZerosMSB(b);
      if (s0 > 0) begin
	 let shift = (s0 - b0);
	 if ((shift & 1) == 1)
	    shift = shift + 1;
	 b = b >> shift;
      end

      fFirst.enq(tuple4(tagged Invalid,s,r,b));
   endrule

   FIFO#(Tuple4#(Maybe#(Bit#(m)),Bit#(m),Bit#(m),Bit#(m))) fThis = fFirst;
   FIFO#(Tuple4#(Maybe#(Bit#(m)),Bit#(m),Bit#(m),Bit#(m))) fNext;

   for (Integer i = 0; i < (valueOf(m)/2)/n+1; i = i + 1) begin
      fNext <- mkLFIFO;
      rule work;
	 //match {.res, .s, .r, .b} <- toGet(fThis).get;
	 let x <- toGet(fThis).get;
	 Maybe#(Bit#(m)) res = tpl_1(x);
	 Bit#(m) s = tpl_2(x);
	 Bit#(m) r = tpl_3(x);
	 Bit#(m) b = tpl_4(x);

	 for (Integer j = 0; j < n; j = j + 1) begin
	    if ((i + j) <= (valueOf(m)/2)) begin
	       if (res matches tagged Invalid) begin
		  if (b == 0) begin
		     res = tagged Valid r;
		  end
		  else begin
		     let sum = r + b;

		     if (s >= sum) begin
			s = s - sum;
			r = (r >> 1) + b;
		     end
		     else begin
			r = r >> 1;
		     end

		     b = b >> 2;
		  end
	       end
	    end
	 end

	 fNext.enq(tuple4(res,s,r,b));
      endrule
      fThis = fNext;
   end

   rule finish;
      match {.res, .s, .r, .b} <- toGet(fThis).get;

      fResponse.enq(tuple2(unpack(fromMaybe(0,res)),(s != 0)));
   endrule

   interface request = toPut(fRequest);
   interface response = toGet(fResponse);

endmodule

module mkFixedPointSquareRooter#(Integer n)(Server#(FixedPoint#(isize,fsize),Tuple2#(FixedPoint#(isize,fsize),Bool)))
   provisos(
      Add#(isize,fsize,m),
      // per request of bsc
      Add#(a__, 2, m),
      Add#(b__, TLog#(TAdd#(1, m)), TAdd#(TLog#(m), 1)),
      Log#(TAdd#(1, m), TLog#(TAdd#(m, 1))),
      Add#(c__, TLog#(TAdd#(m, 1)), TAdd#(TLog#(m), 1))
      );

   FIFO#(FixedPoint#(isize,fsize)) fRequest <- mkLFIFO;
   FIFO#(Tuple2#(FixedPoint#(isize,fsize),Bool)) fResponse <- mkLFIFO;

   Server#(UInt#(m),Tuple2#(UInt#(m),Bool)) sqrt <- mkSquareRooter(n);
   FIFO#(Int#(TAdd#(TLog#(m),1))) fShift <- mkLFIFO;

   rule start;
      let op <- toGet(fRequest).get;
      UInt#(m) value = unpack({op.i,op.f});

      let zeros = countZerosMSB(pack(value));
      Int#(TAdd#(TLog#(m),1)) shift;

      // align input
      value = value << (zeros - 1);

      // compute shift for output
      shift = (fromInteger(valueOf(isize)) - extend(unpack(pack(zeros)))) >> 1;
      shift = shift + 1;
      if ((shift & 1) == 0) begin
	 value = value >> 1;
      end

      shift = fromInteger(valueOf(isize)) - shift;

      sqrt.request.put(value);
      fShift.enq(shift);
   endrule

   rule finish;
      match {.result, .inexact} <- sqrt.response.get;
      let shift <- toGet(fShift).get;

      // shift result as necessary
      result = result >> shift;

      FixedPoint#(isize,fsize) fx;
      fx.i = truncateLSB(pack(result));
      fx.f = truncate(pack(result));

      fResponse.enq(tuple2(fx,inexact));
   endrule

   interface request = toPut(fRequest);
   interface response = toGet(fResponse);

endmodule

//typedef UInt#(32) SqrtT;
typedef UInt#(64) SqrtT;
typedef FixedPoint#(32,32) SqrtTFx;

module mkTb(Empty);
   Server#(SqrtT,Tuple2#(SqrtT,Bool)) sqrtm <- mkSquareRooter(1);
   Server#(SqrtTFx,Tuple2#(SqrtTFx,Bool)) sqrtfxm <- mkFixedPointSquareRooter(1);
   FIFOF#(Tuple4#(SqrtT,SqrtT,SqrtTFx,SqrtTFx)) fCheck <- mkLFIFOF;

   function Action testSqrtPipe(Real n);
      action
	 SqrtT ni = fromInteger(trunc(n));
	 SqrtT ri = fromInteger(round(sqrt(n)));
	 SqrtTFx nfx = fromReal(n);
	 SqrtTFx rfx = fromReal(sqrt(n));
	 sqrtm.request.put(ni);
	 sqrtfxm.request.put(nfx);
	 fCheck.enq(tuple4(ni,ri,nfx,rfx));
      endaction
   endfunction

   Stmt test =
   seq
      testSqrtPipe(1);
      testSqrtPipe(2);
      testSqrtPipe(4);
      testSqrtPipe(10);
      //testSqrtPipe(123456789);
      testSqrtPipe('h2000000);
      testSqrtPipe('h3d80000);
      testSqrtPipe('h3ef9db0);
      testSqrtPipe('h3ffffff);
      testSqrtPipe('h3ffc000);
      testSqrtPipe('hfffffffe_00000000);
      testSqrtPipe('hf0000000_00000000);
      testSqrtPipe('hffff0000_00000000);
      testSqrtPipe('hfffe0000);

      testSqrtPipe(0.5);
      testSqrtPipe(0.25);

      while (fCheck.notEmpty)
	 noAction;
   endseq;

   rule check;
      match {.n,.r,.nfx,.rfx} <- toGet(fCheck).get;
      match {.rr, .inexact} <- sqrtm.response.get;
      match {.rrfx, .inexactfx} <- sqrtfxm.response.get;

      if (r != rr) begin
	 $display("sqrt(%d) = %d (expected %d)", n, rr, r);
      end
      else begin
	 $display("sqrt(%d) = %d", n, rr);
      end

      if (rfx != rrfx) begin
	 $display("sqrtfx(", fshow(nfx), ") = ", fshow(rfx), " (expected ", fshow(rrfx) ,")");
      end
      else begin
	 $display("sqrtfx(", fshow(nfx), ") = ", fshow(rfx));
      end
   endrule

   mkAutoFSM(test);

endmodule

endpackage
