`include "ProcConfig.bsv"
import Types::*;
import ProcTypes::*;
import ReservationStationEhr::*;
import SynthParam::*;

typedef struct {
    ExecFunc execFunc;
} FpuMulDivRSData deriving(Bits, Eq, FShow);

// FPU MUL DIV pipeline is aggressive, i.e. it recv bypass and early RS wakeup
typedef ReservationStation#(`RS_FPUMULDIV_SIZE, WrAggrPortNum, FpuMulDivRSData) ReservationStationFpuMulDiv;
(* synthesize *)
module mkReservationStationFpuMulDiv(ReservationStationFpuMulDiv);
    let m <- mkReservationStation(`LAZY_RS_RF, `RS_LAZY_ENQ);
    return m;
endmodule
