`include "ProcConfig.bsv"
import ReorderBuffer::*;
import SynthParam::*;

// macro SUP_ROB is controlled by Makefile, currently should not be defined

typedef ReorderBufferRowEhr#(AluExeNum) RobRowSynth;
(* synthesize *)
module mkRobRowSynth(RobRowSynth);
    let m <- mkReorderBufferRowEhr;
    return m;
endmodule

`ifdef SUP_ROB
// use superscalar ROB

typedef SupReorderBuffer#(AluExeNum) ReorderBufferSynth;

(* synthesize *)
module mkReorderBufferSynth(ReorderBufferSynth);
    let m <- mkSupReorderBuffer(`ROB_LAZY_ENQ, mkRobRowSynth);
    return m;
endmodule

`else

error("Single scalar is not maintained");

`endif
