
interface SatDownCounter#(type t);
    method Action dec;
    method t _read;
    method Action _write(t v);
endinterface

module mkSatDownCounter#(t initVal)(SatDownCounter#(t)) provisos(
    Alias#(t, Bit#(w))
);
    Reg#(t) cnt <- mkReg(initVal);

    PulseWire decEn <- mkPulseWire;
    RWire#(t) wrEn <- mkRWire;

    (* fire_when_enabled, no_implicit_conditions *)
    rule canon;
        t val = cnt;
        if(decEn && val > 0) begin
            val = val - 1;
        end
        if(wrEn.wget matches tagged Valid .v) begin
            val = v;
        end
        cnt <= val;
    endrule

    method dec = decEn.send;
    method _read = cnt._read;
    method _write = wrEn.wset;
endmodule

