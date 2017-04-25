`include "ProcConfig.bsv"
import Types::*;
import Vector::*;
import ClientServer::*;

// typedefs for mem access performed by processor
// cache line is defined in CacheUtils.bsv

typedef Data MemResp;

typedef enum{Ld, St, Lr, Sc, Amo} MemOp deriving(Eq,Bits,FShow); // add more ops

Bit#(3) memB    = 3'b000;
Bit#(3) memH    = 3'b001;
Bit#(3) memW    = 3'b010;
Bit#(3) memD    = 3'b011;
Bit#(3) memBU   = 3'b100;
Bit#(3) memHU   = 3'b101;
Bit#(3) memWU   = 3'b110;

