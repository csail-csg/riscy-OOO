`include "ProcConfig.bsv"
import Types::*;
import ProcTypes::*;
import ReservationStationEhr::*;
import SynthParam::*;

typedef struct {
    MemFunc mem_func;
    ImmData imm;
    LdStQTag ldstq_tag;
} MemRSData deriving(Bits, Eq, FShow);

// MEM pipeline is aggressive, i.e. it recv bypass and early RS wakeup
typedef ReservationStation#(`RS_MEM_SIZE, WrAggrPortNum, MemRSData) ReservationStationMem;
(* synthesize *)
module mkReservationStationMem(ReservationStationMem);
    let m <- mkReservationStation(`LAZY_RS_RF, `RS_LAZY_ENQ);
    return m;
endmodule
