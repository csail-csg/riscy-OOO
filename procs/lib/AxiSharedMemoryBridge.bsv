`include "ProcConfig.bsv"
import Types::*;
import ClientServer::*;
import ConnectalConfig::*;
import GetPut::*;
import Fifo::*;
import MemoryTypes::*;
import RegFile::*;
import Vector::*;
import FIFO::*;
import FIFOF::*;
import HostInterface::*;
import MemTypes::*;
import CacheUtils::*; // for PendMemRespCnt
import BRAMFIFO::*;
import Axi4Interconnect::*;
import Axi4MasterSlave::*;
import DefaultValue::*;

interface AxiSharedMemoryBridge;
  // Processor Interface
  interface RiscyAxiSlave                 to_proc;

  // Shared Memory Interfaces
  interface MemReadClient#(DataBusWidth)  to_host_read;
  interface MemWriteClient#(DataBusWidth) to_host_write;

  // Initialize the shared memory with the ref pointer and size.
  // If an address is out of range, it will handled (somehow)
  method Action initSharedMem(Bit#(32) refPointer, Addr memSize);

  // Methods for clearing pending requests before reset
  method Action flushRespReqMem;
  method PendMemRespCnt numberFlyingOperations;
endinterface

(* synthesize *)
module mkAxiSharedMemoryBridge(AxiSharedMemoryBridge);
  // TODO: re-implement WORKAROUND_ISSUE_27
  `ifdef WORKAROUND_ISSUE_27
  error("WORKAROUND_ISSUE_27 is not implemented for mkAxiSharedMemoryBridge");
  `endif
  // TODO: re-implement SERIALIZE_MEM_REQS
  `ifdef SERIALIZE_MEM_REQS
  error("SERIALIZE_MEM_REQS is not implemented for mkAxiSharedMemoryBridge");
  `endif

  let verbose = False;

  Reg#(SGLId)                     refPointerReg <- mkReg(0);
  Reg#(Addr)                      memSizeReg    <- mkReg(64 << 20); // 64 MB by default
  RiscyAxiFIFOs                   axiFifos      <- mkAxi4FIFOs;
  FIFO#(MemRequest)               readReqFifo   <- mkFIFO();
  FIFO#(MemRequest)               writeReqFifo  <- mkFIFO();
  FIFO#(MemData#(DataBusWidth))   writeDataFifo <- mkSizedBRAMFIFO(1024);
  FIFO#(Bit#(MemTagSize))         writeDoneFifo <- mkFIFO();
  FIFO#(MemData#(DataBusWidth))   readDataFifo  <- mkFIFO();

  // For tracking pending requests before resetting
  Reg#(PendMemRespCnt) flyingOperations <- mkReg(0);
  Reg#(Bool) flushRespReq <- mkReg(False);

  // addr aligned with 8B boundary
  function Addr getDWordAlignAddr(Addr a);
      return {truncateLSB(a), 3'b0};
  endfunction

  // This function adjusts the address to point to a valid location in memory
  // If the memory size is a power of 2, it simply truncates it.
  // Otherwise is uses a weird mask derived form memSizeReg - 1
  function Addr adjustAddress(Addr a);
    // This works really well if the address is a power of 2, otherwise it has
    // weird behavior (but still functions as desired).
    let memSizeMask = memSizeReg - 1;
    // If the address needs adjusting, and it with memSizeMask
    return (a >= memSizeReg) ? (a & memSizeMask) : a;
  endfunction

  rule transferReadRequest;
    let r = axiFifos.readReq.first;
    axiFifos.readReq.deq;
    Addr addr = adjustAddress(getDWordAlignAddr(r.address));
    flyingOperations <= flyingOperations + 1;
    readReqFifo.enq(MemRequest { sglId: refPointerReg, offset: truncate(addr), burstLen: (zeroExtend(r.len) + 1) << 3, tag: 1});
  endrule
  rule transferReadData;
    let d = readDataFifo.first;
    readDataFifo.deq;
    RiscyAxiReadResponse axi_read_resp = defaultValue;
    axi_read_resp.data = d.data;
    axi_read_resp.last = pack(d.last);
    if (d.last) begin
      flyingOperations <= flyingOperations - 1;
    end
    axiFifos.readResp.enq(axi_read_resp);
  endrule
  rule transferWriteRequest;
    let r = axiFifos.writeReq.first;
    axiFifos.writeReq.deq;
    Addr addr = adjustAddress(getDWordAlignAddr(r.address));
    writeReqFifo.enq(MemRequest { sglId: refPointerReg, offset: truncate(addr), burstLen: (zeroExtend(r.len) + 1) << 3, tag: 0});
  endrule
  rule transferWriteData;
    let d = axiFifos.writeData.first;
    axiFifos.writeData.deq;
    writeDataFifo.enq(MemData { data: d.data, tag: 0, last: unpack(d.last) });
  endrule
  rule transferWriteResponse;
    let r = writeDoneFifo.first;
    writeDoneFifo.deq;
    RiscyAxiWriteResponse axi_write_resp = defaultValue;
    axiFifos.writeResp.enq(axi_write_resp);
  endrule

  interface RiscyAxiSlave to_proc = toAxi4Slave(axiFifos);

  interface MemReadClient to_host_read;
    interface Get readReq = toGet(readReqFifo);
    interface Put readData = toPut(readDataFifo);
  endinterface

  interface MemWriteClient to_host_write;
    interface Get writeReq = toGet(writeReqFifo);
    interface Get writeData = toGet(writeDataFifo);
    interface Put writeDone = toPut(writeDoneFifo);
  endinterface

  method Action initSharedMem(Bit#(32) refPointer, Addr memSize);
    refPointerReg <= refPointer;
    memSizeReg <= memSize;
  endmethod

  method Action flushRespReqMem;
    if (verbose) $display("Flying memory request when stop: %d",flyingOperations);
    flushRespReq <= True;
    noAction;
  endmethod
  method PendMemRespCnt numberFlyingOperations;
    return flyingOperations;
  endmethod
endmodule
