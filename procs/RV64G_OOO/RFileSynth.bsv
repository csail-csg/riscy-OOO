`include "ProcConfig.bsv"
import PhysRFile::*;
import SynthParam::*;

typedef RFile#(RFileWrPortNum, RFileRdPortNum) RFileSynth;

(* synthesize *)
module mkRFileSynth(RFileSynth);
    let m <- mkRFile(`LAZY_RS_RF);
    return m;
endmodule
