`include "ProcConfig.bsv"
// Fpu.bsv
// This file contains modules needed for hardware floating-point arithmetic.
// The FpuExec module abstracts away the ISA implementation simplifying the
// requirements for the hardware FPU units.

import Types::*;
import ProcTypes::*;
import Fifo::*;
import ClientServer::*;
import GetPut::*;
import Divide_Riscy::*;
import SquareRoot_Riscy::*;
import FloatingPoint_Riscy::*;
// FloatingPoint.bsv is provided by Bluespec, FloatingPoint_Riscy.bsv is our modified version
// Included Types:
//    struct  FloatingPoint#(exponent, mantissa) = {sign, exp, sfd}
//    enum    Disorder                           = {LT, EQ, GT, UO}
//    enum    FpuRoundMode                       = {Rnd_Nearest_Even, Rnd_Nearest_Away_Zero, Rnd_Plus_Inf, Rnd_Minus_Inf, Rnd_Zero}
//    enum    FpuException                       = {invalid_op, di, vide_0, overflow, underflow, inexact}
//    typedef Float                              = FloatingPoint#(8, 23)
//    typedef Double                             = FloatingPoint#(11, 52)
// Included Functions:
//    function Tuple2#(FloatingPoint#(e2,m2),FpuException) convert(FloatingPoint#(e,m) in, FpuRoundMode rmode, Bool preservenan)
//    // Zero extend a quantity by padding on the LSB side.
//    function Bit#(m) zExtendLSB(Bit#(n) value)
//    // Zero extend and change type by padding on the LSB side (of bits instance)
//    function a cExtendLSB(b value)
//    // Returns the 1-based index (or 0 if not found) of the first 1
//    // from the MSB down.
//    function Integer findIndexOneMSB_( Bit#(s) din );
//    function UInt#(l) findIndexOneMSB( Bit#(s) din )
//    // Returns the 1-based index (or 0 if not found) of the first 1
//    // from the LSB up.
//    function UInt#(l) findIndexOneLSB( Bit#(s) din )
//    function Integer findIndexOneLSB_( Bit#(s) din );
//    function Bool isInfinity( FloatingPoint#(e,m) din );
//    function Bool isQNaN( FloatingPoint#(e,m) din );
//    function Bool isSNaN( FloatingPoint#(e,m) din );
//    function Bool isZero( FloatingPoint#(e,m) din ); // + and -
//    function Bool isSubNormal( FloatingPoint#(e,m) din );
//    function Disorder compareFP( FloatingPoint#(e,m) x, FloatingPoint#(e,m) y );
//    function Tuple2#(FloatingPoint#(e,m),FpuException) addFP ( FloatingPoint#(e,m) in1, FloatingPoint#(e,m) in2, FpuRoundMode rmode )
//    function Tuple2#(FloatingPoint#(e,m),FpuException) multFP ( FloatingPoint#(e,m) in1, FloatingPoint#(e,m) in2, FpuRoundMode rmode )
//    function Tuple2#(FloatingPoint#(e,m),FpuException) divFP ( FloatingPoint#(e,m) in1, FloatingPoint#(e,m) in2, FpuRoundMode rmode )
//    function Tuple2#(FloatingPoint#(e,m),FpuException) sqrtFP (FloatingPoint#(e,m) in1, FpuRoundMode rmode)

typedef struct {
    Data              data;
    Bit#(5)           fflags;
} FpuResult deriving(Bits, Eq, FShow);

interface FpuExec;
    method Action       exec(FpuInst fInst, Data rVal1, Data rVal2, Data rVal3);
    method Bool         notEmpty; // True if there is any instruction in this pipeline
    // output
    method Bool         result_rdy;
    method FpuResult    result_data;
    method Action       result_deq;
endinterface

function Tuple2#(FloatingPoint#(e,m), FpuException) fmaFP(FloatingPoint#(e,m) in1, FloatingPoint#(e,m) in2, FloatingPoint#(e,m) in3, FpuRoundMode rmode)
    provisos (Add#(a__, TLog#(TAdd#(1, TAdd#(m, 5))), TAdd#(e, 1)),
      Add#(b__, TLog#(TAdd#(1, TAdd#(TAdd#(m, 1), TAdd#(m, 1)))), TAdd#(e, 1)));
  let {mult_data, mult_exception} = multFP(in1, in2, rmode);
  let {final_data, add_exception} = addFP(mult_data, in3, rmode);
  return tuple2(final_data, mult_exception | add_exception);
endfunction

// Double Precision FPU Pipelines
`ifndef REUSE_FMA
// If reusing the FMA, don't bother compiling these.
// Hopefully this reduces compilation/synthesis time.
(* synthesize *)
module mkDoubleAdd(Server#(Tuple3#(Double, Double, FpuRoundMode), Tuple2#(Double, FpuException)));
  let fpu <- mkFloatingPointAdder;
  return fpu;
endmodule
(* synthesize *)
module mkDoubleMult(Server#(Tuple3#(Double, Double, FpuRoundMode), Tuple2#(Double, FpuException)));
  let fpu <- mkFloatingPointMultiplier;
  return fpu;
endmodule
`endif
(* synthesize *)
module mkDoubleDiv(Server#(Tuple3#(Double, Double, FpuRoundMode), Tuple2#(Double, FpuException)));
  let int_div <- mkDivider(1);
  let fpu <- mkFloatingPointDivider(int_div);
  return fpu;
endmodule
(* synthesize *)
module mkDoubleSqrt(Server#(Tuple2#(Double, FpuRoundMode), Tuple2#(Double, FpuException)));
  let int_sqrt <- mkSquareRooter(1);
  let fpu <- mkFloatingPointSquareRooter(int_sqrt);
  return fpu;
endmodule
(* synthesize *)
module mkDoubleFMA(Server#(Tuple4#(Maybe#(Double), Double, Double, FpuRoundMode), Tuple2#(Double, FpuException)));
  let fpu <- mkFloatingPointFusedMultiplyAccumulate;
  return fpu;
endmodule
// Single Precision FPU Pipelines
// These aren't used anymore, so don't synthesize them
// (* synthesize *)
module mkFloatAdd(Server#(Tuple3#(Float, Float, FpuRoundMode), Tuple2#(Float, FpuException)));
  let fpu <- mkFloatingPointAdder;
  return fpu;
endmodule
// (* synthesize *)
module mkFloatMult(Server#(Tuple3#(Float, Float, FpuRoundMode), Tuple2#(Float, FpuException)));
  let fpu <- mkFloatingPointMultiplier;
  return fpu;
endmodule
// (* synthesize *)
module mkFloatDiv(Server#(Tuple3#(Float, Float, FpuRoundMode), Tuple2#(Float, FpuException)));
  let int_div <- mkDivider(1);
  let fpu <- mkFloatingPointDivider(int_div);
  return fpu;
endmodule
// (* synthesize *)
module mkFloatSqrt(Server#(Tuple2#(Float, FpuRoundMode), Tuple2#(Float, FpuException)));
  let int_sqrt <- mkSquareRooter(1);
  let fpu <- mkFloatingPointSquareRooter(int_sqrt);
  return fpu;
endmodule
// (* synthesize *)
module mkFloatFMA(Server#(Tuple4#(Maybe#(Float), Float, Float, FpuRoundMode), Tuple2#(Float, FpuException)));
  let fpu <- mkFloatingPointFusedMultiplyAccumulate;
  return fpu;
endmodule

module mkFloatWrapperForBinaryOp#(Server#(Tuple3#(Double, Double, FpuRoundMode),Tuple2#(Double, FpuException)) double_fpu)(Server#(Tuple3#(Float, Float, FpuRoundMode), Tuple2#(Float, FpuException)));
  let verbose = False;
  Fifo#(2, Tuple2#(FpuRoundMode,FpuException)) rmode_exc_fifo <- mkCFFifo;
  interface Put request;
    method Action put(Tuple3#(Float, Float, FpuRoundMode) x);
      let rmode = tpl_3(x);
      Double d1; FpuException exc1;
      Double d2; FpuException exc2;
      {d1, exc1} = convert(tpl_1(x), rmode, True);
      {d2, exc2} = convert(tpl_2(x), rmode, True);
      double_fpu.request.put(tuple3(d1, d2, rmode));
      rmode_exc_fifo.enq(tuple2(rmode, exc1 | exc2));
      if (verbose) $display("    exc1 = ", fshow(exc1));
      if (verbose) $display("    exc2 = ", fshow(exc2));
    endmethod
  endinterface
  interface Get response;
    method ActionValue#(Tuple2#(Float, FpuException)) get;
      rmode_exc_fifo.deq;
      let {rmode, exc_in} = rmode_exc_fifo.first;
      let x <- double_fpu.response.get;
      let exc_op = tpl_2(x);
      Float f; FpuException exc_conv;
      {f, exc_conv} = convert(tpl_1(x), rmode, True);
      if (verbose) $display("    exc_in = ", fshow(exc_in));
      if (verbose) $display("    exc_op = ", fshow(exc_op));
      if (verbose) $display("    exc_conv = ", fshow(exc_conv));
      return tuple2(f, exc_in | exc_op | exc_conv);
    endmethod
  endinterface
endmodule
module mkFloatWrapperForSqrt#(Server#(Tuple2#(Double, FpuRoundMode),Tuple2#(Double, FpuException)) double_fpu)(Server#(Tuple2#(Float, FpuRoundMode), Tuple2#(Float, FpuException)));
  Fifo#(2, Tuple2#(FpuRoundMode,FpuException)) rmode_exc_fifo <- mkCFFifo;
  interface Put request;
    method Action put(Tuple2#(Float, FpuRoundMode) x);
      let rmode = tpl_2(x);
      Double d1; FpuException exc1;
      {d1, exc1} = convert(tpl_1(x), rmode, True);
      double_fpu.request.put(tuple2(d1, rmode));
      rmode_exc_fifo.enq(tuple2(rmode, exc1 ));
    endmethod
  endinterface
  interface Get response;
    method ActionValue#(Tuple2#(Float, FpuException)) get;
      rmode_exc_fifo.deq;
      let {rmode, exc_in} = rmode_exc_fifo.first;
      let x <- double_fpu.response.get;
      let exc_op = tpl_2(x);
      Float f; FpuException exc_conv;
      {f, exc_conv} = convert(tpl_1(x), rmode, True);
      return tuple2(f, exc_in | exc_op | exc_conv);
    endmethod
  endinterface
endmodule
module mkFloatWrapperForFMA#(Server#(Tuple4#(Maybe#(Double), Double, Double, FpuRoundMode),Tuple2#(Double, FpuException)) double_fpu)(Server#(Tuple4#(Maybe#(Float), Float, Float, FpuRoundMode), Tuple2#(Float, FpuException)));
  Fifo#(2, Tuple2#(FpuRoundMode,FpuException)) rmode_exc_fifo <- mkCFFifo;
  interface Put request;
    method Action put(Tuple4#(Maybe#(Float), Float, Float, FpuRoundMode) x);
      let rmode = tpl_4(x);
      Maybe#(Double) d1_maybe = tagged Invalid;
      FpuException exc1 = unpack(0);
      if (isValid(tpl_1(x))) begin
        Double d1;
        {d1, exc1} = convert(fromMaybe(?,tpl_1(x)), rmode, True);
        d1_maybe = tagged Valid d1;
      end
      Double d2; FpuException exc2;
      Double d3; FpuException exc3;
      {d2, exc2} = convert(tpl_2(x), rmode, True);
      {d3, exc3} = convert(tpl_3(x), rmode, True);
      double_fpu.request.put(tuple4(d1_maybe, d2, d3, rmode));
      rmode_exc_fifo.enq(tuple2(rmode, exc1 | exc2 | exc3));
    endmethod
  endinterface
  interface Get response;
    method ActionValue#(Tuple2#(Float, FpuException)) get;
      rmode_exc_fifo.deq;
      let {rmode, exc_in} = rmode_exc_fifo.first;
      let x <- double_fpu.response.get;
      let exc_op = tpl_2(x);
      Float f; FpuException exc_conv;
      {f, exc_conv} = convert(tpl_1(x), rmode, True);
      return tuple2(f, exc_in | exc_op | exc_conv);
    endmethod
  endinterface
endmodule
module mkAddWrapperForFMA#(Server#(Tuple4#(Maybe#(ft), ft, ft, FpuRoundMode), Tuple2#(ft, FpuException)) fma_module)
                            (Server#(Tuple3#(ft, ft, FpuRoundMode), Tuple2#(ft, FpuException)))
                            provisos(Alias#(ft, FloatingPoint#(e,m)));
    interface Put request;
        method Action put(Tuple3#(ft, ft, FpuRoundMode) x);
            let in1 = tpl_1(x);
            let in2 = tpl_2(x);
            let rm = tpl_3(x);
            ft one_const = one(False);
            fma_module.request.put(tuple4(tagged Valid in1, in2, one_const, rm));
        endmethod
    endinterface
    interface Get response;
        method ActionValue#(Tuple2#(ft, FpuException)) get;
            let x <- fma_module.response.get();
            return x;
        endmethod
    endinterface
endmodule
module mkMulWrapperForFMA#(Server#(Tuple4#(Maybe#(ft), ft, ft, FpuRoundMode), Tuple2#(ft, FpuException)) fma_module)
                            (Server#(Tuple3#(ft, ft, FpuRoundMode), Tuple2#(ft, FpuException)))
                            provisos(Alias#(ft, FloatingPoint#(e,m)));
    interface Put request;
        method Action put(Tuple3#(ft, ft, FpuRoundMode) x);
            let in1 = tpl_1(x);
            let in2 = tpl_2(x);
            let rm = tpl_3(x);
            fma_module.request.put(tuple4(tagged Invalid, in1, in2, rm));
        endmethod
    endinterface
    interface Get response;
        method ActionValue#(Tuple2#(ft, FpuException)) get;
            let x <- fma_module.response.get();
            return x;
        endmethod
    endinterface
endmodule

// FCVT float -> float functions
function Tuple2#(Double, FpuException) fcvt_d_s (Float in, FpuRoundMode rmode);
  return convert(in, rmode, True);
endfunction
function Tuple2#(Float, FpuException) fcvt_s_d (Double in, FpuRoundMode rmode);
  return convert(in, rmode, True);
endfunction

// FCVT float -> int functions
function Tuple2#(Bit#(64), FpuException) fcvt_l_f (FloatingPoint#(e,m) in, FpuRoundMode rmode);
  return float_to_int(in, False, False, rmode);
endfunction
function Tuple2#(Bit#(64), FpuException) fcvt_lu_f (FloatingPoint#(e,m) in, FpuRoundMode rmode);
  return float_to_int(in, False, True, rmode);
endfunction
function Tuple2#(Bit#(64), FpuException) fcvt_w_f (FloatingPoint#(e,m) in, FpuRoundMode rmode);
  return float_to_int(in, True, False, rmode);
endfunction
function Tuple2#(Bit#(64), FpuException) fcvt_wu_f (FloatingPoint#(e,m) in, FpuRoundMode rmode);
  return float_to_int(in, True, True, rmode);
endfunction

// FCVT int -> float functions
function Tuple2#(FloatingPoint#(e,m), FpuException) fcvt_f_l (Bit#(64) in_bits, FpuRoundMode rmode)
    provisos (FixedFloatCVT#(FloatingPoint#(e, m), Int#(64)));
  Int#(64) in = unpack(in_bits);
  return vFixedToFloat(in, 1'b0, rmode);
endfunction
function Tuple2#(FloatingPoint#(e,m), FpuException) fcvt_f_lu (Bit#(64) in_bits, FpuRoundMode rmode)
    provisos (FixedFloatCVT#(FloatingPoint#(e, m), UInt#(64)));
  UInt#(64) in = unpack(in_bits);
  return vFixedToFloat(in, 1'b0, rmode);
endfunction
function Tuple2#(FloatingPoint#(e,m), FpuException) fcvt_f_w (Bit#(64) in_bits, FpuRoundMode rmode)
    provisos (FixedFloatCVT#(FloatingPoint#(e, m), Int#(32)));
  Int#(32) in = unpack(truncate(in_bits));
  return vFixedToFloat(in, 1'b0, rmode);
endfunction
function Tuple2#(FloatingPoint#(e,m), FpuException) fcvt_f_wu (Bit#(64) in_bits, FpuRoundMode rmode)
    provisos (FixedFloatCVT#(FloatingPoint#(e, m), UInt#(32)));
  UInt#(32) in = unpack(truncate(in_bits));
  return vFixedToFloat(in, 1'b0, rmode);
endfunction

function Tuple2#(Bit#(64), FpuException) fmin_s(Bit#(64) in1, Bit#(64) in2);
  Float in1_f = unpack(in1[31:0]);
  Float in2_f = unpack(in2[31:0]);
  FpuException e = unpack(0);
  if (isSNaN(in1_f) || isSNaN(in2_f)) begin
    e.invalid_op = True;
  end
  if (isNaN(in2_f)) begin
    return tuple2(in1, e);
  end else if (isNaN(in1_f)) begin
    return tuple2(in2, e);
  end else begin
	  let signLT = (in1_f.sign && !in2_f.sign);
	  let signEQ = in1_f.sign == in2_f.sign;
    let absLT = {in1_f.exp, in1_f.sfd} < {in2_f.exp, in2_f.sfd};
    if (signLT || (signEQ && (in1_f.sign ? !absLT : absLT))) begin
      return tuple2(in1, e);
    end else begin
      return tuple2(in2, e);
    end
  end
endfunction

function Tuple2#(Bit#(64), FpuException) fmin_d(Bit#(64) in1, Bit#(64) in2);
  Double in1_f = unpack(in1);
  Double in2_f = unpack(in2);
  FpuException e = unpack(0);
  if (isSNaN(in1_f) || isSNaN(in2_f)) begin
    e.invalid_op = True;
  end
  if (isNaN(in2_f)) begin
    return tuple2(in1, e);
  end else if (isNaN(in1_f)) begin
    return tuple2(in2, e);
  end else begin
	  let signLT = (in1_f.sign && !in2_f.sign);
	  let signEQ = in1_f.sign == in2_f.sign;
    let absLT = {in1_f.exp, in1_f.sfd} < {in2_f.exp, in2_f.sfd};
    if (signLT || (signEQ && (in1_f.sign ? !absLT : absLT))) begin
      return tuple2(in1, e);
    end else begin
      return tuple2(in2, e);
    end
  end
endfunction

function Tuple2#(Bit#(64), FpuException) fmax_s(Bit#(64) in1, Bit#(64) in2);
  Float in1_f = unpack(in1[31:0]);
  Float in2_f = unpack(in2[31:0]);
  FpuException e = unpack(0);
  if (isSNaN(in1_f) || isSNaN(in2_f)) begin
    e.invalid_op = True;
  end
  if (isNaN(in2_f)) begin
    return tuple2(in1, e);
  end else if (isNaN(in1_f)) begin
    return tuple2(in2, e);
  end else begin
	  let signGT = (!in1_f.sign && in2_f.sign);
	  let signEQ = in1_f.sign == in2_f.sign;
    let absGT = {in1_f.exp, in1_f.sfd} > {in2_f.exp, in2_f.sfd};
    if (signGT || (signEQ && (in1_f.sign ? !absGT : absGT))) begin
      return tuple2(in1, e);
    end else begin
      return tuple2(in2, e);
    end
  end
endfunction

function Tuple2#(Bit#(64), FpuException) fmax_d(Bit#(64) in1, Bit#(64) in2);
  Double in1_f = unpack(in1);
  Double in2_f = unpack(in2);
  FpuException e = unpack(0);
  if (isSNaN(in1_f) || isSNaN(in2_f)) begin
    e.invalid_op = True;
  end
  if (isNaN(in2_f)) begin
    return tuple2(in1, e);
  end else if (isNaN(in1_f)) begin
    return tuple2(in2, e);
  end else begin
	  let signGT = (!in1_f.sign && in2_f.sign);
	  let signEQ = in1_f.sign == in2_f.sign;
    let absGT = {in1_f.exp, in1_f.sfd} > {in2_f.exp, in2_f.sfd};
    if (signGT || (signEQ && (in1_f.sign ? !absGT : absGT))) begin
      return tuple2(in1, e);
    end else begin
      return tuple2(in2, e);
    end
  end
endfunction

function Tuple2#(Bit#(64), FpuException) float_to_int(FloatingPoint#(e, m) in, Bool is_32bit, Bool is_unsigned, FpuRoundMode rmode);
  // 3 cases of exponents:
  Bit#(64) out = 0;
  Bit#(64) max_val = is_unsigned ? '1 : (is_32bit ? 64'h000000007FFFFFFF : 64'h7FFFFFFFFFFFFFFF);
  Bit#(64) min_val = is_unsigned ? '1 : (is_32bit ? 64'hFFFFFFFF80000000 : 64'h8000000000000000);

  FpuException exc = unpack(0);
  if (isNaN(in) || isInfinity(in)) begin
    out = in.sign ? min_val : max_val;
    exc.invalid_op = True;
  end else if (isZero(in)) begin
    out = 0;
  end else begin
    // Now actually do the conversion
    Int#(TAdd#(e,TLog#(m))) bias_exp = fromInteger((2**(valueOf(e)-1))-1);
    Int#(TAdd#(e,TLog#(m))) in_exp = unpack(zeroExtend(in.exp));
    // The bottom two bits of int_val will be fractional data.
    // The bottom bit holds information about all bits with lesser
    // significance that were shifted out - same for top bit - this is
    // necessary for rounding and overflow detection.
    Bit#(TAdd#(66,m)) int_val = {64'b1, in.sfd, 2'b0}; // this is 2**m times larger than it should be - we will shift this to correct for that
    int_val = saturating_shift_right(int_val, fromInteger(valueOf(m)) + bias_exp - in_exp);
    
    // do rounding
    // 00 : exact
    // 01 : < 0.5
    // 10 : = 0.5
    // 11 : > 0.5
    Bool round_up = False; // by magnitude (default behavior will be drop bits)
    if (int_val[1:0] != 0) begin
      exc.inexact = True;
      case (rmode)
        Rnd_Nearest_Even:       round_up = (int_val[1:0] == 2'b11) || ((int_val[1:0] == 2'b10) && (int_val[2] == 1));
        Rnd_Nearest_Away_Zero:  round_up = (int_val[1] == 1);
        Rnd_Plus_Inf:           round_up = !in.sign;
        Rnd_Minus_Inf:          round_up = in.sign;
        Rnd_Zero:               round_up = False;
      endcase
    end

    // Take the integer part of int_val and round it up if necessary
    Bit#(TAdd#(64,m)) int_val_rnd = truncateLSB(int_val) + (round_up ? 1 : 0);
    // correct the output sign for negative numbers rounded to 0
    Bool out_sign = (int_val_rnd == 0) ? False : in.sign;

    // Now check to see if int_val_rnd is in range
    Bit#(TAdd#(64,m)) mask_32bit = {0, 32'hFFFFFFFF};
    Bit#(TAdd#(64,m)) mask_64bit = {0, 64'hFFFFFFFFFFFFFFFF};
    if (is_unsigned) begin
      if (out_sign) begin
        // negative number - out of range
        out = '1;
        exc.invalid_op = True;
      end else begin
        // positive number
        if (is_32bit) begin
          // WU
          if ((int_val_rnd & mask_32bit) == int_val_rnd) begin
            Bit#(32) val = truncate(int_val_rnd);
            out = signExtend(val);
          end else begin
            // out of range
            out = '1;
            exc.invalid_op = True;
          end
        end else begin
          // LU
          if ((int_val_rnd & mask_64bit) == int_val_rnd) begin
            out = truncate(int_val_rnd);
          end else begin
            // out of range
            out = '1;
            exc.invalid_op = True;
          end
        end
      end
    end else begin
      // signed
      if (is_32bit) begin
        // W
        Bit#(32) max_val = out_sign ? 32'h80000000 : 32'h7FFFFFFF;
        if (((int_val_rnd & mask_32bit) == int_val_rnd) && (truncate(int_val_rnd) <= max_val)) begin
          Bit#(32) val = truncate(int_val_rnd);
          val = out_sign ? ((~val) + 1) : val;
          out = signExtend(val);
        end else begin
          // out of range
          out = signExtend(max_val);
          exc.invalid_op = True;
        end
      end else begin
        // L
        Bit#(64) max_val = out_sign ? 64'h8000000000000000 : 64'h7FFFFFFFFFFFFFFF;
        if (((int_val_rnd & mask_64bit) == int_val_rnd) && (truncate(int_val_rnd) <= max_val)) begin
          Bit#(64) val = truncate(int_val_rnd);
          out = out_sign ? ((~val) + 1) : val;
        end else begin
          // out of range
          out = max_val;
          exc.invalid_op = True;
        end
      end
    end
  end
  if (exc.invalid_op) begin
    // by convention
    exc.inexact = False;
  end
  return tuple2(out, exc);
endfunction

function Bit#(n) saturating_shift_right(Bit#(n) in, Int#(m) amt)
    provisos (Add#(a__,1,n));
  // This function saturates in each direction
  Bool amt_sign = msb(amt) == 1;
  Bit#(m) amt_abs = amt_sign ? ((~pack(amt))+1) : pack(amt);
  Bit#(n) shifted = amt_sign ? (in << amt_abs) : (in >> amt_abs);
  Bit#(n) shifted_out_mask = amt_sign ? ~('1 >> amt_abs) : ~('1 << amt_abs);
  Bit#(1) saturated_bit = |(in & shifted_out_mask);
  shifted = amt_sign ? (shifted | {saturated_bit, 0}) : (shifted | {0, saturated_bit});
  return shifted;
endfunction

function Tuple2#(FloatingPoint#(e, m), FpuException) int_to_float( Bit#(65) in, Bool is_32bit, Bool is_unsigned, FpuRoundMode rmode )
    provisos (FixedFloatCVT#(FloatingPoint#(e, m), Int#(65)));
  Int#(65) in_signed;
  if (is_32bit && is_unsigned) begin
    in_signed = unpack(zeroExtend(in[31:0]));
  end else if (is_32bit && !is_unsigned) begin
    in_signed = unpack(signExtend(in[31:0]));
  end else if(!is_32bit && is_unsigned) begin
    in_signed = unpack(zeroExtend(in));
  end else if(!is_32bit && !is_unsigned) begin
    in_signed = unpack(signExtend(in));
  end
  return vFixedToFloat(in_signed, 1'b0, rmode);
endfunction

// FIXME: finish or remove
// function Tuple2#(Data, FpuException) apply_float_op( function a f(Float x1, Float x2), Data rVal1, Data rVal2 );
//     Float f1 = unpack(truncate(rVal1));
//     Float f2 = unpack(truncate(rVal2));
//     a f_op_res = f(f1,f2);
//     Data res = zeroExtend(pack(f_res));
// endfunction

// exec function for simple operations
(* noinline *)
function FpuResult execFpuSimple(FpuInst fpu_inst, Data rVal1, Data rVal2);
    FpuResult fpu_result = FpuResult{data: 0, fflags: 0};

    // figure out round mode
    let rm = fpu_inst.rm;

    // Convert the Risc-V type RM to FloatingPoint_Riscy::FpuRoundMode
    FpuRoundMode fpu_rm = (case (rm)
            RNE:      Rnd_Nearest_Even;
            RTZ:      Rnd_Zero;
            RDN:      Rnd_Minus_Inf;
            RUP:      Rnd_Plus_Inf;
            RMM:      Rnd_Nearest_Away_Zero;
            RDyn:     Rnd_Nearest_Even;
            default:  Rnd_Nearest_Even;
        endcase);

    if (fpu_inst.precision == Single) begin
        // single precision
        Float in1 = unpack(rVal1[31:0]);
        Float in2 = unpack(rVal2[31:0]);
        Float dst = unpack(0);
        Maybe#(Data) full_dst = Invalid;
        FpuException e = unpack(0);
        let fpu_f = fpu_inst.func;
        // Fpu Decoding
        case (fpu_f)
            // combinational instructions
            FMin:     begin
                Data x;
                {x, e} = fmin_s(rVal1, rVal2);
                full_dst = tagged Valid x;
            end
            FMax:     begin
                Data x;
                {x, e} = fmax_s(rVal1, rVal2);
                full_dst = tagged Valid x;
            end
            FEq:        dst = unpack(zeroExtend(pack(in1 == in2)));
            FLt:        begin
                dst = unpack(zeroExtend(pack(in1 < in2)));
                if (isNaN(in1) || isNaN(in2)) begin
                    e.invalid_op = True;
                end
            end
            FLe:        begin
                dst = unpack(zeroExtend(pack(in1 <= in2)));
                if (isNaN(in1) || isNaN(in2)) begin
                    e.invalid_op = True;
                end
            end
            // CLASS functions
            FClass: begin
                Bool exp_0s = (in1.exp == 0);
                Bool exp_1s = (in1.exp == '1);
                Bool sfd_0s = (in1.sfd == 0);
                Bit#(10) res = 0;
                res[0] = pack(in1.sign && exp_1s && sfd_0s);                // -inf
                res[1] = pack(in1.sign && !exp_1s && !exp_0s);              // -normal
                res[2] = pack(in1.sign && exp_0s && !sfd_0s);               // -subnormal
                res[3] = pack(in1.sign && exp_0s && sfd_0s);                // -0
                res[4] = pack(!in1.sign && exp_0s && sfd_0s);               // +0
                res[5] = pack(!in1.sign && exp_0s && !sfd_0s);              // +subnormal
                res[6] = pack(!in1.sign && !exp_1s && !exp_0s);             // +normal
                res[7] = pack(!in1.sign && exp_1s && sfd_0s);               // -inf
                res[8] = pack(exp_1s && !sfd_0s && (msb(in1.sfd) == 0));    // signaling NaN
                res[9] = pack(exp_1s && !sfd_0s && (msb(in1.sfd) == 1));    // quiet NaN
                full_dst = tagged Valid zeroExtend(res);
            end
            // Sign Injection
            FSgnj:    begin
                dst = in1;
                dst.sign = in2.sign;
            end
            FSgnjn: begin
                dst = in1;
                dst.sign = !in2.sign;
            end
            FSgnjx: begin
                dst = in1;
                dst.sign = unpack(pack(in1.sign) ^ pack(in2.sign));
            end
            // Float -> Bits
            FMv_XF:     full_dst = tagged Valid signExtend(pack(in1));
            // Bits -> Float
            FMv_FX:     full_dst = tagged Valid zeroExtend(pack(in1));
            // Float -> Float
            FCvt_FF:    begin
                Double in1_double = unpack(rVal1);
                {dst, e} = fcvt_s_d(in1_double, fpu_rm);
            end
            // Float -> Int
            FCvt_WF:    begin
                Data dst_bits;
                {dst_bits, e} = fcvt_w_f(in1, fpu_rm);
                full_dst = tagged Valid dst_bits;
            end
            FCvt_WUF: begin
                Data dst_bits;
                {dst_bits, e} = fcvt_wu_f(in1, fpu_rm);
                full_dst = tagged Valid dst_bits;
            end
            FCvt_LF:    begin
                Data dst_bits;
                {dst_bits, e} = fcvt_l_f(in1, fpu_rm);
                full_dst = tagged Valid dst_bits;
            end
            FCvt_LUF: begin
                Data dst_bits;
                {dst_bits, e} = fcvt_lu_f(in1, fpu_rm);
                full_dst = tagged Valid dst_bits;
            end
            // Int -> Float
            FCvt_FW:    {dst, e} = fcvt_f_w(rVal1, fpu_rm);
            FCvt_FWU: {dst, e} = fcvt_f_wu(rVal1, fpu_rm);
            FCvt_FL:    {dst, e} = fcvt_f_l(rVal1, fpu_rm);
            FCvt_FLU: {dst, e} = fcvt_f_lu(rVal1, fpu_rm);
        endcase
        fpu_result.data = (full_dst matches tagged Valid .data ? data : zeroExtend(pack(dst)));
        fpu_result.fflags = pack(e);
    end else if (fpu_inst.precision == Double) begin
        // double precision
        Double in1 = unpack(rVal1);
        Double in2 = unpack(rVal2);
        Double dst = unpack(0);
        Maybe#(Data) full_dst = Invalid;
        FpuException e = unpack(0);
        let fpu_f = fpu_inst.func;
        // Fpu Decoding
        case (fpu_f)
            // combinational instructions
            FMin:     begin
                Data x;
                {x, e} = fmin_d(rVal1, rVal2);
                full_dst = tagged Valid x;
            end
            FMax:     begin
                Data x;
                {x, e} = fmax_d(rVal1, rVal2);
                full_dst = tagged Valid x;
            end
            FEq:        dst = unpack(zeroExtend(pack(in1 == in2)));
            FLt:        begin
                dst = unpack(zeroExtend(pack(in1 < in2)));
                if (isNaN(in1) || isNaN(in2)) begin
                    e.invalid_op = True;
                end
            end
            FLe:        begin
                dst = unpack(zeroExtend(pack(in1 <= in2)));
                if (isNaN(in1) || isNaN(in2)) begin
                    e.invalid_op = True;
                end
            end
            // CLASS functions
            FClass: begin
                Bool exp_0s = (in1.exp == 0);
                Bool exp_1s = (in1.exp == '1);
                Bool sfd_0s = (in1.sfd == 0);
                Bit#(10) res = 0;
                res[0] = pack(in1.sign && exp_1s && sfd_0s);                // -inf
                res[1] = pack(in1.sign && !exp_1s && !exp_0s);              // -normal
                res[2] = pack(in1.sign && exp_0s && !sfd_0s);               // -subnormal
                res[3] = pack(in1.sign && exp_0s && sfd_0s);                // -0
                res[4] = pack(!in1.sign && exp_0s && sfd_0s);               // +0
                res[5] = pack(!in1.sign && exp_0s && !sfd_0s);              // +subnormal
                res[6] = pack(!in1.sign && !exp_1s && !exp_0s);             // +normal
                res[7] = pack(!in1.sign && exp_1s && sfd_0s);               // -inf
                res[8] = pack(exp_1s && !sfd_0s && (msb(in1.sfd) == 0));    // signaling NaN
                res[9] = pack(exp_1s && !sfd_0s && (msb(in1.sfd) == 1));    // quiet NaN
                full_dst = tagged Valid zeroExtend(res);
            end
            // Sign Injection
            FSgnj:    begin
                dst = in1;
                dst.sign = in2.sign;
            end
            FSgnjn: begin
                dst = in1;
                dst.sign = !in2.sign;
            end
            FSgnjx: begin
                dst = in1;
                dst.sign = unpack(pack(in1.sign) ^ pack(in2.sign));
            end
            // Float -> Bits
            FMv_XF:     full_dst = tagged Valid pack(in1);
            // Bits -> Float
            FMv_FX:     full_dst = tagged Valid pack(in1);
            // Float -> Float
            FCvt_FF:    begin
                Float in1_float = unpack(rVal1[31:0]);
                {dst, e} = fcvt_d_s(in1_float, fpu_rm);
            end
            // Float -> Int
            FCvt_WF:    begin
                Data dst_bits;
                {dst_bits, e} = fcvt_w_f(in1, fpu_rm);
                full_dst = tagged Valid dst_bits;
            end
            FCvt_WUF: begin
                Data dst_bits;
                {dst_bits, e} = fcvt_wu_f(in1, fpu_rm);
                full_dst = tagged Valid dst_bits;
            end
            FCvt_LF:    begin
                Data dst_bits;
                {dst_bits, e} = fcvt_l_f(in1, fpu_rm);
                full_dst = tagged Valid dst_bits;
            end
            FCvt_LUF: begin
                Data dst_bits;
                {dst_bits, e} = fcvt_lu_f(in1, fpu_rm);
                full_dst = tagged Valid dst_bits;
            end
            // Int -> Float
            FCvt_FW:    {dst, e} = fcvt_f_w(rVal1, fpu_rm);
            FCvt_FWU: {dst, e} = fcvt_f_wu(rVal1, fpu_rm);
            FCvt_FL:    {dst, e} = fcvt_f_l(rVal1, fpu_rm);
            FCvt_FLU: {dst, e} = fcvt_f_lu(rVal1, fpu_rm);
        endcase
        fpu_result.data = (full_dst matches tagged Valid .data ? data : pack(dst));
        fpu_result.fflags = pack(e);
    end
    return fpu_result;
endfunction

(* synthesize *)
module mkFpuExecPipeline(FpuExec);
  Fifo#(2, FpuResult) fpu_exec_fifo <- mkCFFifo; // in parallel with pipelined FPUs
  Fifo#(2, FpuInst) fpu_func_fifo <- mkCFFifo; // in parallel with pipelined FPUs
  Fifo#(2, FpuResult) fpu_exec_fifo_out <- mkCFFifo; // all pipelined FPUs dequeue into this

  // Pipelined units
  // Double
  let double_fma <- mkDoubleFMA;
`ifdef REUSE_FMA
  let double_add <- mkAddWrapperForFMA(double_fma);
  let double_mult <- mkMulWrapperForFMA(double_fma);
`else
  let double_add <- mkDoubleAdd;
  let double_mult <- mkDoubleMult;
`endif
  let double_div <- mkDoubleDiv;
  let double_sqrt <- mkDoubleSqrt;

  // // Float
  // let float_add <- mkFloatAdd;
  // let float_mult <- mkFloatMult;
  // let float_div <- mkFloatDiv;
  // let float_sqrt <- mkFloatSqrt;
  // let float_fma <- mkFloatFMA;

  // Float ops implemented with Double FPUs
  let float_fma <- mkFloatWrapperForFMA(double_fma);
  let float_add <- mkFloatWrapperForBinaryOp(double_add);
  let float_mult <- mkFloatWrapperForBinaryOp(double_mult);
  let float_div <- mkFloatWrapperForBinaryOp(double_div);
  let float_sqrt <- mkFloatWrapperForSqrt(double_sqrt);

  rule finish;
    let x = fpu_exec_fifo.first;
    let fpu_inst = fpu_func_fifo.first;
    let fpu_f = fpu_inst.func;
    fpu_exec_fifo.deq;
    fpu_func_fifo.deq;

    if (fpu_inst.precision == Single) begin
      Float out = unpack(0);
      FpuException exc = unpack(0);
      Bool pipeline_result = False;
      // Fpu Decoding
      case (fpu_f)
        // pipeline instructions
        FAdd:   begin {out, exc} <- float_add.response.get; pipeline_result = True; end
        FSub:   begin {out, exc} <- float_add.response.get; pipeline_result = True; end
        FMul:   begin {out, exc} <- float_mult.response.get; pipeline_result = True; end
        FDiv:   begin {out, exc} <- float_div.response.get; pipeline_result = True; end
        FSqrt:  begin {out, exc} <- float_sqrt.response.get; pipeline_result = True; end
        FMAdd:  begin {out, exc} <- float_fma.response.get; pipeline_result = True; end
        FMSub:  begin {out, exc} <- float_fma.response.get; pipeline_result = True; end
        FNMSub: begin {out, exc} <- float_fma.response.get; out = -out; pipeline_result = True; end
        FNMAdd: begin {out, exc} <- float_fma.response.get; out = -out; pipeline_result = True; end
      endcase
      if (pipeline_result) begin
        // canonicalize NaNs
        if (isNaN(out)) out = unpack('1);
        // update data and exception in x
        x.data = zeroExtend(pack(out));
        x.fflags = pack(exc);
      end
    end else if (fpu_inst.precision == Double) begin
      Double out = unpack(0);
      FpuException exc = unpack(0);
      Bool pipeline_result = False;
      // Fpu Decoding
      case (fpu_f)
        // pipeline instructions
        FAdd:   begin {out, exc} <- double_add.response.get; pipeline_result = True; end
        FSub:   begin {out, exc} <- double_add.response.get; pipeline_result = True; end
        FMul:   begin {out, exc} <- double_mult.response.get; pipeline_result = True; end
        FDiv:   begin {out, exc} <- double_div.response.get; pipeline_result = True; end
        FSqrt:  begin {out, exc} <- double_sqrt.response.get; pipeline_result = True; end
        FMAdd:  begin {out, exc} <- double_fma.response.get; pipeline_result = True; end
        FMSub:  begin {out, exc} <- double_fma.response.get; pipeline_result = True; end
        FNMSub: begin {out, exc} <- double_fma.response.get; out = -out; pipeline_result = True; end
        FNMAdd: begin {out, exc} <- double_fma.response.get; out = -out; pipeline_result = True; end
      endcase
      if (pipeline_result) begin
        // canonicalize NaNs
        if (isNaN(out)) out = unpack('1);
        // update data and exception in x
        x.data = pack(out);
        x.fflags = pack(exc);
      end
    end
    fpu_exec_fifo_out.enq(x);
  endrule

  method Action exec(FpuInst fpu_inst, Data rVal1, Data rVal2, Data rVal3);
    // figure out round mode
    let rm = fpu_inst.rm;

    // Convert the Risc-V type RM to FloatingPoint_Riscy::FpuRoundMode
    FpuRoundMode fpu_rm = (case (rm)
        RNE:      Rnd_Nearest_Even;
        RTZ:      Rnd_Zero;
        RDN:      Rnd_Minus_Inf;
        RUP:      Rnd_Plus_Inf;
        RMM:      Rnd_Nearest_Away_Zero;
        RDyn:     Rnd_Nearest_Even;
        default:  Rnd_Nearest_Even;
      endcase);

    FpuResult fpu_result = execFpuSimple(fpu_inst, rVal1, rVal2);

    if (fpu_inst.precision == Single) begin
      // single precision
      Float in1 = unpack(rVal1[31:0]);
      Float in2 = unpack(rVal2[31:0]);
      Float in3 = unpack(rVal3[31:0]);
      Float dst = unpack(0);
      Maybe#(Data) full_dst = Invalid;
      FpuException e = unpack(0);
      let fpu_f = fpu_inst.func;
      // Fpu Decoding
      case (fpu_f)
        // pipeline instructions
        FAdd:   float_add.request.put(tuple3(in1, in2, fpu_rm));
        FSub:   float_add.request.put(tuple3(in1, -in2, fpu_rm));
        FMul:   float_mult.request.put(tuple3(in1, in2, fpu_rm));
        FDiv:   float_div.request.put(tuple3(in1, in2, fpu_rm));
        FSqrt:  float_sqrt.request.put(tuple2(in1, fpu_rm));
        FMAdd:  float_fma.request.put(tuple4(tagged Valid in3, in1, in2, fpu_rm));
        FMSub:  float_fma.request.put(tuple4(tagged Valid (-in3), in1, in2, fpu_rm));
        FNMSub: float_fma.request.put(tuple4(tagged Valid (-in3), in1, in2, fpu_rm));
        FNMAdd: float_fma.request.put(tuple4(tagged Valid in3, in1, in2, fpu_rm));
      endcase
    end else if (fpu_inst.precision == Double) begin
      // double precision
      Double in1 = unpack(rVal1);
      Double in2 = unpack(rVal2);
      Double in3 = unpack(rVal3);
      Double dst = unpack(0);
      Maybe#(Data) full_dst = Invalid;
      FpuException e = unpack(0);
      let fpu_f = fpu_inst.func;
      // Fpu Decoding
      case (fpu_f)
        // pipeline instructions
        FAdd:   double_add.request.put(tuple3(in1, in2, fpu_rm));
        FSub:   double_add.request.put(tuple3(in1, -in2, fpu_rm));
        FMul:   double_mult.request.put(tuple3(in1, in2, fpu_rm));
        FDiv:   double_div.request.put(tuple3(in1, in2, fpu_rm));
        FSqrt:  double_sqrt.request.put(tuple2(in1, fpu_rm));
        FMAdd:  double_fma.request.put(tuple4(tagged Valid in3, in1, in2, fpu_rm));
        FMSub:  double_fma.request.put(tuple4(tagged Valid (-in3), in1, in2, fpu_rm));
        FNMSub: double_fma.request.put(tuple4(tagged Valid (-in3), in1, in2, fpu_rm));
        FNMAdd: double_fma.request.put(tuple4(tagged Valid in3, in1, in2, fpu_rm));
      endcase
    end
    fpu_exec_fifo.enq(fpu_result);
    fpu_func_fifo.enq(fpu_inst);
  endmethod

  method Bool     notEmpty = fpu_exec_fifo_out.notEmpty;
  // output
  method Bool     result_rdy = fpu_exec_fifo_out.notEmpty;
  method FpuResult result_data = fpu_exec_fifo_out.first;
  method Action   result_deq = fpu_exec_fifo_out.deq;
endmodule

(* synthesize *)
module mkFpuExecDummy(FpuExec);
    Fifo#(2, FpuResult) fpu_exec_fifo <- mkCFFifo;

    method Action exec(FpuInst fpu_inst, Data rVal1, Data rVal2, Data rVal3);
        $fdisplay(stderr, "[WARNING] mkFpuExecDummy is in use");
        // don't do the function...
        FpuResult res = unpack(0);
        // ...and enqueue it into fpu_exec_fifo
        fpu_exec_fifo.enq(res);
    endmethod

    method Bool         notEmpty = fpu_exec_fifo.notEmpty;
    // output
    method Bool       result_rdy = fpu_exec_fifo.notEmpty;
    method FpuResult result_data = fpu_exec_fifo.first;
    method Action     result_deq = fpu_exec_fifo.deq;
endmodule
