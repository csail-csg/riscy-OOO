
/*
A Fibonacci Linear Feedback Shift Register (LFSR)
=====================================

A pseudo-random sequence generator

Parameters
----------

| Parameter | Default | Explanation |
|-----------|:-------:|------------:|
| `WIDTH` | 32 | Bitwidth of the LFSR sequence. |
| `POLYNOMIAL` | 32 | Bit indexes to XOR to produce the next bit |
| `SHIFT_RIGHT` | false | If `false`, the sequence sheifts left, and the low-order bit is derived. Else the high-order bit is derived. |
| `INITIAL_VALUE` | 32'b0 | Value the LFSR generator initializes to. |
| `RESET_VALUE` | 32'b0 | Value the LFSR resets to.  |

Interface
---------

                        +------+
  clock           >---->|>     |
  reset           >-----| LFSR |
  enable          >---->|      |>----> out_data
                        +------+

Notes
-----

Aggressively set inputs/parameters to **don't cares** (for example 1'bX)
in order to correctly optimize the design.

An LFSR polynomial generates a sequece of maximum length iff the following holds:
- polynomial is made of an even number of terms
- the polynomial terms are relatively prime

For example,
*/

// ## Implementation
module LFSR (
    clock, reset, enable,
    out_data
  );

  // ### Parameters
  parameter
    WIDTH =         32,
    POLYNOMIAL =    32'h04C11DB7,  // Standard 802.3 CRC32 polynomial
    INITIAL_VALUE = {32{1'b1}}, // `INITIAL_VALUE` must be `LENGTH * `WIDTH` bits wide.
    RESET_VALUE =   {32{1'b1}}; // `RESET_VALUE` must be `LENGTH * `WIDTH` bits wide.

  // ### I/O ports
  input               clock;
  input               reset;
  input               enable;
  output  [WIDTH-1:0] out_data;

  // ### Internal wires
  wire    [WIDTH-1:0] polynomial_xor;
  wire    [WIDTH-1:0] next_data;

  // ### Combinational logic
  //genvar i;
  //generate
  //  for (i = 0; i < WIDTH; i = i+1) begin: polynomial
  //    assign  polynomial_xor[i] =  out_data ^ POLYNOMIAL[i];
  //     end
  //endgenerate
  assign polynomial_xor = {WIDTH{out_data[0]}} & POLYNOMIAL;
  assign next_data = ( {1'b1, {out_data[WIDTH-1:1]}} ^ polynomial_xor);

  // ### Module instantiations
  Register #(
    .WIDTH(           WIDTH),
    .INITIAL_VALUE(   INITIAL_VALUE),
    .RESET_VALUE(     RESET_VALUE)
  ) register (
    .clock(           clock),
    .reset(           reset),
    .enable(          enable),
    .in_data(         next_data),
    .out_data(        out_data) );

endmodule
