
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

import GetPut::*;
import Vector::*;
import FIFO::*;
import Clocks::*;
import Connectable::*;
import Types::*;
import ProcTypes::*;
import ProcIF::*;
import VerificationPacket::*;
import Performance::*;
import Core::*;
import SyncFifo::*;
import MMIOPlatform::*;
import DramLLC::*;

// indication methods that are truly in use by processor
interface ProcIndInv;
    method ActionValue#(Data) to_host;
    method ActionValue#(void) bootRomInitResp;
    method ActionValue#(Tuple2#(CoreId, ProcPerfResp)) perfResp;
    method ActionValue#(CoreId) terminate;
endinterface

instance Connectable#(ProcIndInv, ProcIndication);
    module mkConnection#(ProcIndInv inv, ProcIndication ind)(Empty);
        rule doToHost;
            let v <- inv.to_host;
            ind.to_host(v);
        endrule
        rule doBootRomInitResp;
            let v <- inv.bootRomInitResp;
            ind.bootRomInitResp;
        endrule
        rule doPerf;
            let {c, p} <- inv.perfResp;
            ind.perfResp(zeroExtend(c), p);
        endrule
        rule doTerminate;
            let c <- inv.terminate;
            ind.terminate(zeroExtend(c));
        endrule
    endmodule
endinstance

// this module should be under user clock domain
module mkProcIndInvSync#(
    Vector#(CoreNum, CoreIndInv) inv,
    MMIOPlatform mmio,
    Clock portalClk, Reset portalRst
)(ProcIndInv);
    Clock userClk <- exposeCurrentClock;
    Reset userRst <- exposeCurrentReset;
    SyncFIFOIfc#(Data) hostQ <- mkSyncFifo(
        1, userClk, userRst, portalClk, portalRst
    );
    SyncFIFOIfc#(void) bootRomInitQ <- mkSyncFifo(
        1, userClk, userRst, portalClk, portalRst
    );
    SyncFIFOIfc#(Tuple2#(CoreId, ProcPerfResp)) perfQ <- mkSyncFifo(
        1, userClk, userRst, portalClk, portalRst
    );
    SyncFIFOIfc#(CoreId) terminateQ <- mkSyncFifo(
        1, userClk, userRst, portalClk, portalRst
    );

    rule sendHost;
        let v <- mmio.to_host;
        hostQ.enq(v);
    endrule
    rule sendBootRomInit;
        let v <- mmio.bootRomInitResp;
        bootRomInitQ.enq(?);
    endrule

    for(Integer i = 0; i < valueof(CoreNum); i = i+1) begin
        rule sendPerf;
            let v <- inv[i].perfResp;
            perfQ.enq(tuple2(fromInteger(i), v));
        endrule
        rule sendTerminate;
            let v <- inv[i].terminate;
            terminateQ.enq(fromInteger(i));
        endrule
    end

    method to_host = toGet(hostQ).get;
    method bootRomInitResp = toGet(bootRomInitQ).get;
    method perfResp = toGet(perfQ).get;
    method terminate = toGet(terminateQ).get;
endmodule

// request methods that are truly in use by processor
interface ProcReq;
    method Action start(
        Addr startpc,
        Addr toHostAddr, Addr fromHostAddr,
        DramLatency latency
    );
    method Action from_host(Data v);
    method Action bootRomInitReq(Bit#(16) index, Data v);
    method Action perfReq(Bit#(8) core, PerfLocation loc, PerfType t);
endinterface

// this module should be under user clock domain
module mkProcReqSync#(
    Vector#(CoreNum, CoreReq) req,
    MMIOPlatform mmio, DramLLC dramLLC,
    Clock portalClk, Reset portalRst
)(ProcReq);
    Clock userClk <- exposeCurrentClock;
    Reset userRst <- exposeCurrentReset;
    SyncFIFOIfc#(
        Tuple4#(Addr, Addr, Addr, DramLatency)
    ) startQ <- mkSyncFifo(1, portalClk, portalRst, userClk, userRst);
    SyncFIFOIfc#(Data) hostQ <- mkSyncFifo(
        1, portalClk, portalRst, userClk, userRst
    );
    SyncFIFOIfc#(Tuple2#(BootRomIndex, Data)) bootRomInitQ <- mkSyncFifo(
        1, portalClk, portalRst, userClk, userRst
    );
    SyncFIFOIfc#(Tuple3#(CoreId, PerfLocation, PerfType)) perfQ <- mkSyncFifo(
        1, portalClk, portalRst, userClk, userRst
    );

    rule doStart;
        // broad cast to each core and MMIO platform
        let {pc, toHost, fromHost, latency} <- toGet(startQ).get;
        for(Integer i = 0; i < valueof(CoreNum); i = i+1) begin
            req[i].start(pc, toHost, fromHost);
        end
        mmio.start(toHost, fromHost);
        dramLLC.setLatency(latency);
        // check addr alignment
        doAssert(toHost[2:0] == 0, "tohost addr must be 8B aligned");
        doAssert(fromHost[2:0] == 0, "fromhost addr must be 8B aligned");
    endrule

    rule doHost;
        hostQ.deq;
        let v = hostQ.first;
        mmio.from_host(v);
    endrule

    rule doBootRomInit;
        bootRomInitQ.deq;
        let {idx, v} = bootRomInitQ.first;
        mmio.bootRomInitReq(idx, v);
    endrule

    rule doPerf;
        perfQ.deq;
        let {c, loc, t} = perfQ.first;
        req[c].perfReq(loc, t);
    endrule

    method Action start(
        Addr startpc,
        Addr toHostAddr, Addr fromHostAddr,
        DramLatency latency
    );
        startQ.enq(tuple4(startpc,
                          toHostAddr, fromHostAddr,
                          latency));
    endmethod
    method Action bootRomInitReq(Bit#(16) index, Data v);
        bootRomInitQ.enq(tuple2(truncate(index), v));
    endmethod
    method Action from_host(Data v);
        hostQ.enq(v);
    endmethod
    method Action perfReq(Bit#(8) core, PerfLocation loc, PerfType t);
        perfQ.enq(tuple3(truncate(core), loc, t));
    endmethod
endmodule
