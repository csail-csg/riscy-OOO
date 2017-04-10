import VerificationPacket::*;
import Performance::*;

interface ProcRequest;
  method Action reset();
  method Action start(Bit#(64) startpc, Bool ipi_wait_msip_zero, Bit#(64) verification_packets_to_ignore, Bool send_synchronization_packets);
  //method Action noFlying();
  //method Action stop();
  //method Action verify(Bool v);
  method Action from_host(Bit#(8) core, Bit#(64) v);
  //method Action initSharedMem(Bit#(32) refPointer, Bit#(64) memSize);
  method Action perfReq(Bit#(8) core, PerfLocation loc, PerfType t); // performance
endinterface

interface ProcIndication;
  method Action to_host(Bit#(8) core, Bit#(64) v);

  //method Action debug_pc(Bit#(64) pc);
  //method Action debug_excep(Bit#(64) ex);
  method Action debug_verify(Bit#(8) core, VerificationPacket packet);
  method Action resetDone();
  method Action perfResp(Bit#(8) core, ProcPerfResp r); // performance
  method Action terminate(Bit#(8) core); // exit signal
endinterface
