/*
Register
========
This is the basic state element
Parameters
----------
| Parameter | Default | Explanation |
|-----------|:-------:|------------:|
| `WIDTH` | 32 | Bitwidth of the register |
| `INITIAL_VALUE` | 32'b0 | Value the register is initialized to, if applicable |
| `RESET_VALUE` | 32'b0 | Value the register takes when `reset` is signaled |
Interface
---------
                 +----------+
  clock    >---->|>         |
                 | register |
  reset    >-----|          |
  enable   >---->|          |
  in_data  >---->|          |>----> out_data
                 +----------+
Notes
-----
Priority: Reset > Enable
Aggressively set inputs/parameters to **don't cares** (for example 1'bX)
in order to correctly optimize the design.
*/

// ## Implementation
module Register (
    clock, reset,
    enable,
    in_data, out_data
  );

  // ### Parameters
  parameter
    WIDTH =         32,
    INITIAL_VALUE = {WIDTH{1'b0}},
    RESET_VALUE =   {WIDTH{1'b0}};

  // ### I/O ports
  input               clock;
  input               reset;

  input               enable;
  input   [WIDTH-1:0] in_data;
  output  [WIDTH-1:0] out_data;

  // ### Internal wires
  (* EXTRACT_ENABLE = "yes", EXTRACT_RESET = "yes" *)
  reg     [WIDTH-1:0] register_value;

  // ### Combinational logic
  assign out_data = register_value;

  // ### Initial values
  initial begin
    register_value = INITIAL_VALUE;
    // WARNING:
    // DO NOT USE 'output reg Out = Initial';
    // Vivado 2013.4+ silently ignores this syntax for synthesis.
  end

  // ### Synchronous logic
  always @ (posedge clock) begin
    if (reset)        register_value <= RESET_VALUE;
    else if (enable)  register_value <=  in_data;
  end

endmodule
