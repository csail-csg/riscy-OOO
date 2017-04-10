`include "ProcConfig.bsv"

import Types::*;
import ProcTypes::*;
import Vector::*;
import BrPred::*;
import Bht::*;
import GSelectPred::*;
import TourPred::*;

export DirPredTrainInfo(..);
export mkDirPredictor;

`ifdef DIR_PRED_BHT
typedef BhtTrainInfo DirPredTrainInfo;
`endif
`ifdef DIR_PRED_GSELECT
typedef GSelectTrainInfo DirPredTrainInfo;
`endif
`ifdef DIR_PRED_TOUR
typedef TourTrainInfo DirPredTrainInfo;
`endif

(* synthesize *)
module mkDirPredictor(DirPredictor#(DirPredTrainInfo));
`ifdef DIR_PRED_BHT
    let m <- mkBht;
`endif
`ifdef DIR_PRED_GSELECT
    let m <- mkGSelectPred;
`endif
`ifdef DIR_PRED_TOUR
    let m <- mkTourPred;
`endif
    return m;
endmodule
