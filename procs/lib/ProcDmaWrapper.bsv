
// Copyright (c) 2017 Massachusetts Institute of Technology
// 
// Permission is hereby granted, free of charge, to any person
// obtaining a copy of this software and associated documentation
// files (the "Software"), to deal in the Software without
// restriction, including without limitation the rights to use, copy,
// modify, merge, publish, distribute, sublicense, and/or sell copies
// of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
// 
// The above copyright notice and this permission notice shall be
// included in all copies or substantial portions of the Software.
// 
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
// EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
// MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
// NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
// BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
// ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
// CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.

import Clocks::*;
import Assert::*;
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
import HostDmaIF::*;
import DeadlockIF::*;
import Proc::*;
import ProcTypes::*;
import ProcSync::*;
import DeadlockSync::*;
import RenameDebugIF::*;
import RenameDebugSync::*;
import SyncFifo::*;

// DRAM stuff
import DDR3Wrapper::*;
import DDR3Common::*;
import DDR3TopPins::*;
import UserClkRst::*;
import DDR3LLC::*;
import HostDmaLLC::*;

interface ProcDmaWrapper;
    interface ProcRequest procReq;
    interface HostDmaRequest hostDmaReq;
    interface DeadlockRequest deadlockReq;
`ifndef BSIM
    interface DDR3TopPins pins;
`endif
endinterface

module mkProcDmaWrapper#(
    HostInterface host,
    ProcIndication procInd,
    HostDmaIndication hostDmaInd,
    DeadlockIndication deadlockInd,
    RenameDebugIndication renameDebugInd
)(ProcDmaWrapper);

    // portal clock
    Clock portalClk <- exposeCurrentClock;
    Reset portalRst <- exposeCurrentReset;

    // user clock
`ifndef BSIM
    UserClkRst userClkRst <- mkUserClkRst(`USER_CLK_PERIOD);
    Clock userClk = userClkRst.clk;
    Reset userRst = userClkRst.rst;
`else
    Clock userClk = portalClk;
    Reset userRst = portalRst;
`endif

    // instantiate DDR3
    Clock sys_clk = host.tsys_clk_200mhz_buf;
    Reset sys_rst_n <- mkAsyncResetFromCR(4, sys_clk);
    DDR3Wrapper ddr3Ifc <- mkDDR3Wrapper(sys_clk, sys_rst_n, clocked_by userClk, reset_by userRst);

    // DRAM controller error
    SyncFIFOIfc#(DDR3Err) dramErrQ <- mkSyncFifo(1, userClk, userRst, portalClk, portalRst);
    mkConnection(toPut(dramErrQ).put, ddr3Ifc.user.err);
    rule doDramErr;
        DDR3Err e <- toGet(dramErrQ).get;
        hostDmaInd.dramErr(zeroExtend(pack(e)));
    endrule

    // DRAM initialized bit
    //Reg#(Bool) inited <- mkReg(False); // cannot send before connectal is inited
    //Reg#(Bool) lastDDR3Status <- mkReg(False);
    //rule doDramStatus(inited);
    //    Bool ddr3_init = ddr3Ifc.user.initDone;
    //    if(ddr3_init != lastDDR3Status) begin
    //        lastDDR3Status <= ddr3_init;
    //        hostDmaInd.dramStatus(ddr3_init);
    //    end
    //endrule

    // instantiate processor
    let proc <- mkProc(portalClk, portalRst, clocked_by userClk, reset_by userRst);

    // connect to proc indication
    mkConnection(proc.procIndInv, procInd);

    // connect to host dma indication
    rule doHostDmaRdData;
        let rd <- proc.rdDataToHost;
        hostDmaInd.rdData(rd.data, rd.burstId);
    endrule
    rule doHostDmaWrDone;
        proc.wrDoneToHost;
        hostDmaInd.wrDone;
    endrule

    // connect to DDR3
    mkDDR3LLC(
        ddr3Ifc.user, proc.toDDR3, valueof(DDR3LLCMaxReads), False,
        clocked_by userClk, reset_by userRst
    );
    
    // connect to deadlock
    mkConnection(deadlockInd, proc.deadlockIndInv);

    // connect to rename debug
    mkConnection(renameDebugInd, proc.renameDebugIndInv);
    
    interface ProcRequest procReq;
        method start = proc.procReq.start;
        method from_host = proc.procReq.from_host;
        method perfReq = proc.procReq.perfReq;
        method Action reset;
            // XXX [sizhuo] I am not doing any reset...
            procInd.resetDone;
            // this method tells us that connectal is inited
            //inited <= True;
            $fdisplay(stderr, "[ProcDmaWrapper] WARNING: reset has no effect now");
        endmethod
        //method Action stop;
        //    $fdisplay(stderr, "[ProcDmaWrapper] ERROR: stop is not implemented");
        //    $finish;
        //endmethod
        //method Action noFlying;
        //    $fdisplay(stderr, "[ProcDmaWrapper] ERROR: noFlying is not implemented");
        //    $finish;
        //endmethod
        //method Action initSharedMem(Bit#(32) refPointer, Bit#(64) memSize);
        //    $fdisplay(stderr, "[ProcDmaWrapper] ERROR: use DDR3 now, initSharedMem is obsolete");
        //    $finish;
        //endmethod
    endinterface
    interface hostDmaReq = proc.hostDmaReq;
    interface deadlockReq = proc.deadlockReq;
`ifndef BSIM
    interface DDR3TopPins pins;
        interface ddr3 = ddr3Ifc.ddr3;
    endinterface
`endif
endmodule
