import Types::*;
import ProcTypes::*;

(* noinline *)
function Data amoExec(AmoInst amo_inst, Data current_data, Data in_data, Bool upper_32_bits);
  // upper_32_bits is true if the operation is a 32-bit operation and the
  // address is the upper 32-bits of a 64-bit region.
  Data new_data = 0;
  Data old_data = current_data;

  if (!amo_inst.doubleWord) begin
    if (!upper_32_bits) begin
      current_data = (amo_inst.func == Min || amo_inst.func == Max) ? signExtend(current_data[31:0]) : zeroExtend(current_data[31:0]);
      in_data = (amo_inst.func == Min || amo_inst.func == Max) ? signExtend(in_data[31:0]) : zeroExtend(in_data[31:0]);
    end else begin
      // use upper 32-bits instead
      current_data = (amo_inst.func == Min || amo_inst.func == Max) ? signExtend(current_data[63:32]) : zeroExtend(current_data[63:32]);
      in_data = (amo_inst.func == Min || amo_inst.func == Max) ? signExtend(in_data[31:0]) : zeroExtend(in_data[31:0]);
    end
  end

  case (amo_inst.func)
    Swap: new_data = in_data;
    Add:  new_data = current_data + in_data;
    Xor:  new_data = current_data ^ in_data;
    And:  new_data = current_data & in_data;
    Or:   new_data = current_data | in_data;
    Min:  new_data = sMin(current_data, in_data);
    Max:  new_data = sMax(current_data, in_data);
    Minu: new_data = uMin(current_data, in_data);
    Maxu: new_data = uMax(current_data, in_data);
  endcase

  if (!amo_inst.doubleWord) begin
    if (!upper_32_bits) begin
      return {old_data[63:32], new_data[31:0]};
    end else begin
      // change upper 32-bits instead
      return {new_data[31:0], old_data[31:0]};
    end
  end else begin
    return new_data;
  end
endfunction

function Bit#(t) sMax( Bit#(t) a, Bit#(t) b );
  Int#(t) x = max(unpack(a), unpack(b));
  return pack(x);
endfunction
function Bit#(t) sMin( Bit#(t) a, Bit#(t) b );
  Int#(t) x = min(unpack(a), unpack(b));
  return pack(x);
endfunction
function Bit#(t) uMax( Bit#(t) a, Bit#(t) b );
  UInt#(t) x = max(unpack(a), unpack(b));
  return pack(x);
endfunction
function Bit#(t) uMin( Bit#(t) a, Bit#(t) b );
  UInt#(t) x = min(unpack(a), unpack(b));
  return pack(x);
endfunction
