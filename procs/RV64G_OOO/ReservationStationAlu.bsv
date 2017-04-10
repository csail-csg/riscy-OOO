`include "ProcConfig.bsv"
import Types::*;
import ProcTypes::*;
import ReservationStationEhr::*;
import SynthParam::*;
import BrPred::*;
import DirPredictor::*;

typedef struct {
    DecodedInst dInst;
    DirPredTrainInfo dpTrain;
} AluRSData deriving(Bits, Eq, FShow);

// ALU pipeline is aggressive, i.e. it recv bypass and early RS wakeup
typedef ReservationStation#(`RS_ALU_SIZE, WrAggrPortNum, AluRSData) ReservationStationAlu;
(* synthesize *)
module mkReservationStationAlu(ReservationStationAlu);
    ReservationStationAlu m <- mkReservationStation(`LAZY_RS_RF, `RS_LAZY_ENQ);
    return m;
endmodule
