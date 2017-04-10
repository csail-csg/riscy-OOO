import Clocks::*;
import Connectable::*;
import ConnectalConfig::*;
import FIFO::*;
import GetPut::*;
import HostInterface::*;
import MemTypes::*;
import Pipe::*;
import Vector::*;
import StmtFSM::*;
import ProcIF::*;
import Proc::*;

// generated bsv:
import ProcIndication::*;
import ProcRequest::*;

// FIXME: connectal should generate this
instance Connectable#(ProcIndicationInverse, ProcIndication);
   module mkConnection#(ProcIndicationInverse in, ProcIndication out)(Empty);
      rule rl_to_host;
	 let msg <- toGet(in.to_host).get();
	 out.to_host(msg.v);
      endrule
      rule rl_debug_pc;
	 let msg <- toGet(in.debug_pc).get();
	 out.debug_pc(msg.pc);
      endrule
      rule rl_debug_excep;
	 let msg <- toGet(in.debug_excep).get();
	 out.debug_excep(msg.ex);
      endrule
      rule rl_debug_verify;
    let msg <- toGet(in.debug_verify).get();
    out.debug_verify(msg.packet);
      endrule
      rule rl_resetDone;
	 let msg <- toGet(in.resetDone).get();
	 out.resetDone();
      endrule
      rule rl_perfResp;
        let msg <- in.perfResp;
        out.perfResp(msg.r);
      endrule
   endmodule
endinstance

interface ProcWrapper;
  interface ProcRequest request;
  interface Vector#(1, MemReadClient#(64)) dmaReadClient;
  interface Vector#(1, MemWriteClient#(64)) dmaWriteClient;
endinterface

module mkProcWrapper#(ProcIndication indication)(ProcWrapper);
   let clock <- exposeCurrentClock;
   let reset <- exposeCurrentReset;
   let procReset <- mkReset(10, True, clock);
   let resetSent <- mkFIFO1();

   let proc <- mkProc(reset_by procReset.new_rst);
   mkConnection(proc.indicationInverse, indication);
   
   Stmt waitAndReset =
   (seq
       // $fwrite(stderr,"Start reset\n");
       proc.request.stop();
       // $fwrite(stderr,"Processor stopped, wait for response and flush them\n");
       proc.request.noFlying();
       // $fwrite(stderr,"Responses flushed\n");
       procReset.assertReset();
       resetSent.enq(True);
       // $fwrite(stderr, "mkProc.request.reset\n");
    endseq);
   
   FSM resetMachine <- mkFSM(waitAndReset);
   
   rule rl_in_reset if (!procReset.isAsserted);
      let v <- toGet(resetSent).get();
      indication.resetDone();
      $fwrite(stderr, "mkProc.indication.resetDone\n");
   endrule

   interface ProcRequest request;
      method Action reset();
	 resetMachine.start();
      endmethod
      method stop = proc.request.stop;
      method noFlying = proc.request.noFlying; 
      method start = proc.request.start;
      method from_host = proc.request.from_host;
      method initSharedMem = proc.request.initSharedMem;
      method perfReq = proc.request.perfReq;
   endinterface
   interface dmaReadClient = proc.dmaReadClient;
   interface dmaWriteClient = proc.dmaWriteClient;
endmodule
