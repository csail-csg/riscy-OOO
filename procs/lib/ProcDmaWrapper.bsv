
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
import ClientServer::*;
import HostInterface::*;
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
import DramCommon::*;
import DDR3Common::*;
import AWSDramCommon::*;
import DramWrapper::*;
import UserClkRst::*;
import HostDmaLLC::*;

`ifdef USE_VC707_DRAM
typedef DDR3Err DramErr;
typedef DDR3UserWrapper DramUserWrapper;
typedef DDR3FullWrapper DramFullWrapper;
typedef DDR3_1GB_Pins DramPins;
`endif
`ifdef USE_AWSF1_DRAM
typedef AWSDramErr DramErr;
typedef AWSDramUserWrapper DramUserWrapper;
typedef AWSDramFullWrapper DramFullWrapper;
typedef AWSDramPins DramPins;
`endif

interface ProcDmaWrapper;
    interface ProcRequest procReq;
    interface HostDmaRequest hostDmaReq;
`ifndef BSIM
    interface DramPins pins;
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
`ifdef USE_VC707_DRAM
    Clock sys_clk = host.tsys_clk_200mhz_buf;
    Reset sys_rst_n <- mkAsyncResetFromCR(4, sys_clk);
    DramFullWrapper dram <- mkDDR3Wrapper(
        sys_clk, sys_rst_n, clocked_by userClk, reset_by userRst
    );
`endif
`ifdef USE_AWSF1_DRAM
    DramFullWrapper dram <- mkAWSDramWrapper(
        portalClk, portalRst, clocked_by userClk, reset_by userRst
    );
`endif

    // DRAM controller error
    SyncFIFOIfc#(DramErr) dramErrQ <- mkSyncFifo(1, userClk, userRst, portalClk, portalRst);
    mkConnection(toPut(dramErrQ).put, dram.user.err);
    rule doDramErr;
        DramErr e <- toGet(dramErrQ).get;
        hostDmaInd.dramErr(zeroExtend(pack(e)));
    endrule

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
    mkConnection(proc.toDram.request.get, dram.user.req);
    mkConnection(proc.toDram.response.put, dram.user.rdResp);
    
    // connect to deadlock
    mkConnection(deadlockInd, proc.deadlockIndInv);

    // connect to rename debug
    mkConnection(renameDebugInd, proc.renameDebugIndInv);
    
    interface ProcRequest procReq;
        method start = proc.procReq.start;
        method from_host = proc.procReq.from_host;
        method bootRomInitReq = proc.procReq.bootRomInitReq;
        method perfReq = proc.procReq.perfReq;
        method Action reset;
            // XXX [sizhuo] I am not doing any reset...
            procInd.resetDone;
            // this method tells us that connectal is inited
            //inited <= True;
            $fdisplay(stderr, "[ProcDmaWrapper] WARNING: reset has no effect now");
        endmethod
    endinterface
    interface hostDmaReq = proc.hostDmaReq;
`ifndef BSIM
    interface pins = dram.pins;
`endif
endmodule
