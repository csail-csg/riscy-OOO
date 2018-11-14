import Types::*;

interface TRNGImport;
    method Data read;
endinterface

import "BVI" TRNG =
module mkTRNGImport(TRNGImport);
    default_clock clk(clock, (*unused*)unused_gate);
    default_reset no_reset;

    method out read();
    schedule (read) CF (read);
endmodule

(* synthesize *)
module mkTRNG(Reg#(Data));
    let trngImport <-mkTRNGImport;
    method Data _read;
        return trngImport.read;
    endmethod
    method Action _write(Data x);
        noAction;
    endmethod
endmodule
