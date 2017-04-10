import FloatingPoint_Riscy::*;
// implementation of SQRT in c
import "BDPI" function Float sqrt_float_c(Float in1, FpuRoundMode rmode);
import "BDPI" function Double sqrt_double_c(Double in1, FpuRoundMode rmode);

function Tuple2#(Float, FpuException) sqrt_float(Float in1, FpuRoundMode rmode);
  Float out = sqrt_float_c(in1, rmode);
  FpuException exc = unpack(0);
  if ( (|(out.sfd[4:0])) == 1 ) begin
    // if any of the bottom 5 bits are 1, then the result is probably inexact
    exc.inexact = True;
  end
  return tuple2(out, exc);
endfunction

function Tuple2#(Double, FpuException) sqrt_double(Double in1, FpuRoundMode rmode);
  Double out = sqrt_double_c(in1, rmode);
  FpuException exc = unpack(0);
  // faking it
  if ( (|(out.sfd[4:0])) == 1 ) begin
    // if any of the bottom 5 bits are 1, then the result is probably inexact
    exc.inexact = True;
  end
  return tuple2(out, exc);
endfunction
