
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
import LLCache::*;
import DramLLC::*;
import Performance::*;

// indication methods that are truly in use by processor
interface ProcIndInv;
    method ActionValue#(Data) to_host;
    method ActionValue#(Tuple2#(Bit#(8), ProcPerfResp)) perfResp;
    method ActionValue#(CoreId) terminate;
endinterface

instance Connectable#(ProcIndInv, ProcIndication);
    module mkConnection#(ProcIndInv inv, ProcIndication ind)(Empty);
        rule doToHost;
            let v <- inv.to_host;
            ind.to_host(v);
        endrule
        rule doPerf;
            let {c, p} <- inv.perfResp;
            ind.perfResp(c, p);
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
    MMIOPlatform mmio, LLCache llc,
    Clock portalClk, Reset portalRst
)(ProcIndInv);
    Clock userClk <- exposeCurrentClock;
    Reset userRst <- exposeCurrentReset;
    SyncFIFOIfc#(Data) hostQ <- mkSyncFifo(
        1, userClk, userRst, portalClk, portalRst
    );
    SyncFIFOIfc#(Tuple2#(Bit#(8), ProcPerfResp)) perfQ <- mkSyncFifo(
        1, userClk, userRst, portalClk, portalRst
    );
    SyncFIFOIfc#(CoreId) terminateQ <- mkSyncFifo(
        1, userClk, userRst, portalClk, portalRst
    );

    rule sendHost;
        let v <- mmio.to_host;
        hostQ.enq(v);
    endrule

    rule sendLLCPerf;
        let r <- llc.perf.resp;
        // core id of uncore = core num
        perfQ.enq(tuple2(fromInteger(valueof(CoreNum)), ProcPerfResp {
            loc: LLC,
            pType: zeroExtend(pack(r.pType)),
            data: r.data
        }));
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
    method perfResp = toGet(perfQ).get;
    method terminate = toGet(terminateQ).get;
endmodule

// this module should be under user clock domain
module mkProcReqSync#(
    Vector#(CoreNum, CoreReq) req,
    MMIOPlatform mmio, LLCache llc, DramLLC dramLLC,
    Clock portalClk, Reset portalRst
)(ProcRequest);
    Clock userClk <- exposeCurrentClock;
    Reset userRst <- exposeCurrentReset;
    SyncFIFOIfc#(
        Tuple4#(Addr, Addr, Addr, Addr)
    ) startQ <- mkSyncFifo(1, portalClk, portalRst, userClk, userRst);
    SyncFIFOIfc#(Data) hostQ <- mkSyncFifo(
        1, portalClk, portalRst, userClk, userRst
    );
    SyncFIFOIfc#(Tuple3#(Bit#(8), PerfLocation, PerfType)) perfQ <- mkSyncFifo(
        1, portalClk, portalRst, userClk, userRst
    );

    rule doStart;
        // broad cast to each core, MMIO platform, and DRAM
        let {pc, toHost, fromHost, addrMask} <- toGet(startQ).get;
        for(Integer i = 0; i < valueof(CoreNum); i = i+1) begin
            req[i].start(pc, toHost, fromHost);
        end
        mmio.start(toHost, fromHost);
        dramLLC.start(addrMask);
        // check addr alignment
        doAssert(toHost[2:0] == 0, "tohost addr must be 8B aligned");
        doAssert(fromHost[2:0] == 0, "fromhost addr must be 8B aligned");
    endrule

    rule doHost;
        hostQ.deq;
        let v = hostQ.first;
        mmio.from_host(v);
    endrule

    rule doPerf;
        perfQ.deq;
        let {c, loc, t} = perfQ.first;
        if(c >= fromInteger(valueof(CoreNum))) begin
            // uncore
            llc.perf.req(unpack(truncate(t)));
        end
        else begin
            req[c].perfReq(loc, t);
        end
    endrule

    method Action start(
        Addr startpc,
        Addr toHostAddr, Addr fromHostAddr,
        Addr addrOverflowMask
    );
        startQ.enq(tuple4(startpc, toHostAddr, fromHostAddr, addrOverflowMask));
    endmethod
    method Action from_host(Data v);
        hostQ.enq(v);
    endmethod
    method Action perfReq(Bit#(8) core, PerfLocation loc, PerfType t);
        perfQ.enq(tuple3(core, loc, t));
    endmethod
endmodule
