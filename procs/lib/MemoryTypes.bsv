`include "ProcConfig.bsv"
import Types::*;
import Vector::*;
import ClientServer::*;
import Axi4MasterSlave::*;  // from connectal
import Axi4Interconnect::*; // our implementation of an interconnect

// cache line is defined in CacheUtils.bsv

// FIXME: some code seems to rely on Data = Line = WideLine

typedef Data MemResp;

typedef enum{Ld, St, Lr, Sc, Amo} MemOp deriving(Eq,Bits,FShow); // add more ops
typedef struct{
    MemOp           op;
    ByteEn          byteEn;
    Addr            addr;
    Data            data;
    Maybe#(AmoInst) amoInst;
} MemReq deriving(Eq, Bits, FShow);

typedef 64 WideLineSz;
typedef Bit#(WideLineSz) WideLine;
typedef WideLine WideMemResp;

typedef TDiv#(WideLineSz, 8) WideNumBytes;
typedef TDiv#(WideLineSz, DataSz) WideNumData;
typedef TLog#(WideNumBytes) WideIndxShamt;
typedef Vector#(WideNumBytes, Bool) WideByteEn;

typedef struct {
  MemOp op;
  WideByteEn byteEn;
  Addr addr;
  WideLine data;
} WideMemReq deriving(Eq, Bits);

interface WideMem;
  interface Server#(WideMemReq, WideMemResp) to_proc;
  interface Server#(WideMemReq, WideMemResp) to_host;
endinterface

typedef enum {InstMem, DataMem} MemPort deriving (Bits, Eq);

Bit#(3) memB    = 3'b000;
Bit#(3) memH    = 3'b001;
Bit#(3) memW    = 3'b010;
Bit#(3) memD    = 3'b011;
Bit#(3) memBU   = 3'b100;
Bit#(3) memHU   = 3'b101;
Bit#(3) memWU   = 3'b110;

typedef 16 NumTokens;
typedef Bit#(TLog#(NumTokens)) Token;

typedef 16 LoadBufferSz;
typedef Bit#(TLog#(LoadBufferSz)) LoadBufferIndex;

interface MemoryToHost;
  method ActionValue#(Tuple2#(MemPort, MemReq)) req;
  method Action resp(Tuple2#(MemPort, MemResp) r);
endinterface

(* synthesize *)
module mkDummyMemoryToHost(MemoryToHost);
  method ActionValue#(Tuple2#(MemPort, MemReq)) req if(False);
    return ?;
  endmethod

  method Action resp(Tuple2#(MemPort, MemResp) r);
  endmethod
endmodule

interface WideMemoryToHost;
  method ActionValue#(Tuple2#(MemPort, WideMemReq)) req;
  method Action resp(Tuple2#(MemPort, WideMemResp) r);
endinterface

(* synthesize *)
module mkDummyWideMemoryToHost(WideMemoryToHost);
  method ActionValue#(Tuple2#(MemPort, WideMemReq)) req if(False);
    return ?;
  endmethod

  method Action resp(Tuple2#(MemPort, WideMemResp) r);
  endmethod
endmodule

// RiscyAxi types
//////////////////

typedef 64 RiscyAxiAddrWidth;
typedef 64 RiscyAxiBusWidth;
typedef 4  RiscyAxiIdWidth;

typedef Axi4ReadRequest#(RiscyAxiAddrWidth, RiscyAxiIdWidth)  RiscyAxiReadRequest;
typedef Axi4ReadResponse#(RiscyAxiBusWidth, RiscyAxiIdWidth)  RiscyAxiReadResponse;
typedef Axi4WriteRequest#(RiscyAxiAddrWidth, RiscyAxiIdWidth) RiscyAxiWriteRequest;
typedef Axi4WriteData#(RiscyAxiBusWidth, RiscyAxiIdWidth)     RiscyAxiWriteData;
typedef Axi4WriteResponse#(RiscyAxiIdWidth)                   RiscyAxiWriteResponse;

typedef Axi4Master#(RiscyAxiAddrWidth, RiscyAxiBusWidth, RiscyAxiIdWidth) RiscyAxiMaster;
typedef Axi4Slave#(RiscyAxiAddrWidth, RiscyAxiBusWidth, RiscyAxiIdWidth)  RiscyAxiSlave;

typedef Axi4FIFOs#(RiscyAxiAddrWidth, RiscyAxiBusWidth, RiscyAxiIdWidth)  RiscyAxiFIFOs;

typedef Axi4Interconnect#(numMasters, numSlaves, RiscyAxiAddrWidth, RiscyAxiBusWidth, RiscyAxiIdWidth) RiscyAxiInterconnect#(numeric type numMasters, numeric type numSlaves);
