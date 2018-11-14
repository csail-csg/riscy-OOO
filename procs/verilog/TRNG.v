module TRNG(
    clock,
    out
  );

  parameter
    DBITS = 64;

  localparam
    TAPS = 64'hD800_0000_0000_0000;

  input clock;
  output [DBITS-1:0] out;

  // random values from an LFSR for AWS.
  // TODO: find a source of entropy to sample and amplify that can work on AWS!
  LFSR #(
    .WIDTH(           DBITS),
    .POLYNOMIAL(      TAPS),
    .INITIAL_VALUE(   {DBITS{1'b1}})
  ) lfsr (
    .clock(           clock),
    .reset(           1'b0),
    .enable(          1'b1),
    .out_data(        out) );
endmodule
