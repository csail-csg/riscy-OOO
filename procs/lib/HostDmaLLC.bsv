
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
import GetPut::*;
import FShow::*;
import FIFO::*;
import Vector::*;
import Fifo::*;
import Types::*;
import CacheUtils::*;
import CCTypes::*;
import HostDmaIF::*;
import SyncFifo::*;

// transfrom between Host DMA req/resp and LLC DMA req/resp

typedef Bit#(LogMaxHostBurstLen) HostBurstId;

typedef struct {
    HostBurstId firstBurstId; // ID of the first valid burst in this req
    LineDataOffset firstSel; // offset of the first busrt in this req
} HostMemReqId deriving(Bits, Eq, FShow);

typedef DmaRq#(HostMemReqId) HostMemReq;
typedef DmaRs#(HostMemReqId) HostLdResp;

typedef struct {
    HostDmaCmd cmd;
    Bit#(32) addr;
    HostBurstId burstLenMinusOne;
} HostDmaReq deriving(Bits, Eq, FShow);

typedef struct {
    Data data;
    Bit#(DataSzBytes) byteEn;
    Bool last;
} HostDmaWrData deriving(Bits, Eq, FShow);

typedef struct {
    Data data;
    HostBurstId burstId;
} HostDmaRdData deriving(Bits, Eq, FShow);

interface HostMemClient;
    interface FifoDeq#(HostMemReq) memReq;
    interface FifoEnq#(HostLdResp) respLd;
    interface FifoEnq#(HostMemReqId) respSt;
endinterface

interface HostDmaLLC;
    // ifc to LLC, under user clock domain
    interface HostMemClient to_mem;
    // request & indication inverse, under portal clock domain
    interface HostDmaRequest reqFromHost;
    method ActionValue#(HostDmaRdData) rdDataToHost;
    method Action wrDoneToHost;
endinterface

typedef Bit#(TAdd#(1, LogMaxHostBurstLen)) HostBurstLen;

// this module should be clocked under user domain
(* synthesize *)
module mkHostDmaLLC#(Clock portalClk, Reset portalRst)(HostDmaLLC);
    Bool verbose = True;

    Reg#(Maybe#(HostDmaCmd)) curCmd <- mkReg(Invalid);
    PulseWire resetCmd <- mkPulseWire;
    // read counters
    Reg#(HostBurstLen) remainReadReqBurst <- mkRegU;
    Reg#(HostBurstLen) remainReadRespBurst <- mkRegU;
    Reg#(HostBurstId) curReadReqBurstId <- mkRegU;
    // write counters
    Reg#(HostBurstLen) remainWriteReqBurst <- mkRegU;
    Reg#(HostBurstLen) sentLLCStReqCnt <- mkRegU;
    Reg#(HostBurstLen) recvLLCStRespCnt <- mkRegU;
    // format req to LLC
    Reg#(LineDataOffset) reqSel <- mkRegU;
    Reg#(LineAddr) reqAddr <- mkRegU;
    Reg#(Line) reqData <- mkRegU;
    Reg#(Vector#(LineSzData, Bit#(DataSzBytes))) reqBE <- mkRegU;
    // read resp data from LLC
    Reg#(LineDataOffset) respSel <- mkRegU;
    Reg#(HostBurstId) respBurstId <- mkRegU;
    Reg#(Line) respData <- mkRegU;
    Reg#(Bool) respValid <- mkRegU;
    // sync FIFOs to cross to portal clk
    Clock userClk <- exposeCurrentClock;
    Reset userRst <- exposeCurrentReset;
    SyncFIFOIfc#(HostDmaReq) hostReqQ <- mkSyncFifo(1, portalClk, portalRst, userClk, userRst);
    SyncFIFOIfc#(HostDmaWrData) hostWrDataQ <- mkSyncFifo(1, portalClk, portalRst, userClk, userRst);
    SyncFIFOIfc#(HostDmaRdData) hostRdDataQ <- mkSyncFifo(1, userClk, userRst, portalClk, portalRst);
    SyncFIFOIfc#(Bool) hostWrDoneQ <- mkSyncFifo(1, userClk, userRst, portalClk, portalRst);
    // FIFOs to LLC
    Fifo#(2, HostMemReq) memReqQ <- mkCFFifo;
    Fifo#(2, HostLdResp) respLdQ <- mkCFFifo;
    Fifo#(2, HostMemReqId) respStQ <- mkCFFifo;

    rule doNewCmd(curCmd == Invalid);
        hostReqQ.deq;
        HostDmaReq req = hostReqQ.first;
        HostBurstLen totalBurst = zeroExtend(req.burstLenMinusOne) + 1;
        // common setup
        reqSel <= getLineDataOffset(zeroExtend(req.addr));
        reqAddr <= getLineAddr(zeroExtend(req.addr));
        reqBE <= replicate(0);
        curCmd <= Valid (req.cmd);
        // cmd specific setup
        if(req.cmd == HostDmaRead) begin
            remainReadReqBurst <= totalBurst;
            remainReadRespBurst <= totalBurst;
            curReadReqBurstId <= 0;
            respValid <= False;
        end
        else begin
            remainWriteReqBurst <= totalBurst;
            sentLLCStReqCnt <= 0;
            recvLLCStRespCnt <= 0;
        end
        if(verbose) begin
            $display("  [HostDmaLLC doNewCmd] ", fshow(req), " ; total burst %x", totalBurst);
        end
    endrule

    rule doLdReq(curCmd == Valid (HostDmaRead) && remainReadReqBurst > 0);
        // send req to LLC
        HostMemReq req = DmaRq {
            addr: {reqAddr, 0},
            byteEn: replicate(False),
            data: ?,
            id: HostMemReqId {
                firstBurstId: curReadReqBurstId,
                firstSel: reqSel
            }
        };
        memReqQ.enq(req);
        // calculate consumed req bursts
        HostBurstLen consumedBurst = zeroExtend(maxBound - reqSel) + 1;
        // change counters
        reqSel <= 0;
        reqAddr <= reqAddr + 1;
        curReadReqBurstId <= curReadReqBurstId + truncate(consumedBurst);
        if(consumedBurst >= remainReadReqBurst) begin
            remainReadReqBurst <= 0;
            // it's fine for curReadReqBurstId to be wrong in this case
        end
        else begin
            remainReadReqBurst <= remainReadReqBurst - consumedBurst;
        end
        if(verbose) begin
            $display("  [HostDmaLLC doLdReq] ", fshow(req),
                " ; reqSel %d, reqAddr %x, cur id %d, remain burst %d, consume burst %d",
                reqSel, reqAddr, curReadReqBurstId, remainReadReqBurst, consumedBurst
            );
        end
    endrule

    rule doLdResp(curCmd == Valid (HostDmaRead));
        if(respValid) begin
            // we still have data to sent in respData
            let rd = HostDmaRdData {
                data: respData[respSel],
                burstId: respBurstId
            };
            hostRdDataQ.enq(rd);
            // change state
            respSel <= respSel + 1;
            respBurstId <= respBurstId + 1;
            if(respSel == maxBound) begin
                respValid <= False;
            end
            if(verbose) begin
                $display("  [HostDmaLLC doLdResp] use old data: ", fshow(rd),
                    " ; respSel %d, resp burst id %d, respData ",
                    respSel, respBurstId, fshow(respData),
                    " ; remain burst %d", remainReadRespBurst
                );
            end
        end
        else begin
            // we need to get a new resp from LLC
            respLdQ.deq;
            HostLdResp ldResp = respLdQ.first;
            // send a resp to host
            let rd = HostDmaRdData {
                data: ldResp.data[ldResp.id.firstSel],
                burstId: ldResp.id.firstBurstId
            };
            hostRdDataQ.enq(rd);
            // change state
            respData <= ldResp.data;
            respSel <= ldResp.id.firstSel + 1;
            respBurstId <= ldResp.id.firstBurstId + 1;
            respValid <= ldResp.id.firstSel != maxBound;
            if(verbose) begin
                $display("  [HostDmaLLC doLdResp] get new data: ",
                    fshow(ldResp), " ; ", fshow(rd),
                    " ; remain burst %d", remainReadRespBurst
                );
            end
        end
        // reduce resp burst & check cmd finish
        remainReadRespBurst <= remainReadRespBurst - 1;
        if(remainReadRespBurst == 1) begin
            doAssert(remainReadReqBurst == 0, "read req must have been all sent");
            resetCmd.send;
        end
    endrule

    rule doStReq(curCmd == Valid (HostDmaWrite) && remainWriteReqBurst > 0);
        // get write data from host
        hostWrDataQ.deq;
        HostDmaWrData wr = hostWrDataQ.first;
        // merge with req data & BE
        Line newData = reqData;
        Vector#(LineSzData, Bit#(DataSzBytes)) newBE = reqBE;
        newData[reqSel] = wr.data;
        newBE[reqSel] = wr.byteEn;
        // common state update
        reqData <= newData;
        remainWriteReqBurst <= remainWriteReqBurst - 1;
        reqSel <= reqSel + 1;
        reqAddr <= reqSel == maxBound ? reqAddr + 1 : reqAddr;
        // print
        if(verbose) begin
            $display("  [HostDmaLLC doStReq] ", fshow(wr),
                " ; newData ", fshow(newData), " ; newBE " ,fshow(newBE),
                " ; remain burst %d, reqSel %d, reqAddr %x, sent cnt %d",
                remainWriteReqBurst, reqSel, reqAddr, sentLLCStReqCnt
            );
        end
        // we need to req LLC when: (1) fill up a line OR (2) last host busrt
        if(reqSel == maxBound || remainWriteReqBurst == 1) begin
            HostMemReq req = DmaRq {
                addr: {reqAddr, 0},
                byteEn: unpack(pack(newBE)),
                data: newData,
                id: ? // we actually don't care the id
            };
            if(verbose) begin
                $display("  [HostDmaLLC doStReq] send req to LLC ", fshow(req));
            end
            // reset BE for next fresh LLC req
            reqBE <= replicate(0);
            // only send real write to LLC, otherwise may spawn orphan read resp
            if(req.byteEn != replicate(False)) begin
                memReqQ.enq(req);
                sentLLCStReqCnt <= sentLLCStReqCnt + 1;
            end
            else begin
                doAssert(False, "write req cannot have zero BE");
            end
        end
        else begin
            reqBE <= newBE;
        end
        doAssert((remainWriteReqBurst == 1) == wr.last,
            "write last should be consistent with burst len"
        );
    endrule

    rule doStResp(curCmd == Valid (HostDmaWrite));
        // get st resp
        respStQ.deq;
        let recvCntNext = recvLLCStRespCnt + 1;
        recvLLCStRespCnt <= recvCntNext;
        if(recvCntNext == sentLLCStReqCnt && remainWriteReqBurst == 0) begin
            resetCmd.send;
            hostWrDoneQ.enq(?);
        end
        if(verbose) begin
            $display("  [HostDmaLLC doStResp] recv cnt %d, send cnt %d, remain req burst %d",
                recvLLCStRespCnt, sentLLCStReqCnt, remainWriteReqBurst
            );
        end
    endrule

    (* fire_when_enabled, no_implicit_conditions *)
    rule finishCmd(resetCmd && isValid(curCmd));
        curCmd <= Invalid;
        if(verbose) begin
            $display("  [HostDmaLLC finishCmd] reset cmd ", fshow(curCmd), " to Invalid");
        end
    endrule

    interface HostMemClient to_mem;
        interface memReq = toFifoDeq(memReqQ);
        interface respLd = toFifoEnq(respLdQ);
        interface respSt = toFifoEnq(respStQ);
    endinterface

    interface HostDmaRequest reqFromHost;
        method Action req(HostDmaCmd cmd, Bit#(32) addr, HostBurstId burstLenMinusOne);
            hostReqQ.enq(HostDmaReq {
                cmd: cmd,
                addr: addr,
                burstLenMinusOne: burstLenMinusOne
            });
        endmethod
        method Action wrData(Data data, Bit#(DataSzBytes) byteEn, Bool last);
            hostWrDataQ.enq(HostDmaWrData {
                data: data,
                byteEn: byteEn,
                last: last
            });
        endmethod
    endinterface

    method ActionValue#(HostDmaRdData) rdDataToHost;
        let rd <- toGet(hostRdDataQ).get;
        return rd;
    endmethod
    method Action wrDoneToHost;
        hostWrDoneQ.deq;
    endmethod
endmodule
