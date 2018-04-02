
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

import Assert::*;
import FIFO::*;
import BRAMFIFO::*;
import GetPut::*;
import ClientServer::*;
import FShow::*;

import Types::*;
import ProcTypes::*;
import CacheUtils::*;
import CCTypes::*;
import DramCommon::*;
import MMIOAddrs::*;
import LLCache::*;

export DramLLC(..);
export mkDramLLC;

interface DramLLC;
    interface Client#(DramUserReq, DramUserData) toDram;
    method Action setLatency(DramLatency lat);
endinterface

typedef Bit#(TLog#(DramMaxReqs)) DramReqIdx;

module mkDramLLC#(
    MemFifoClient#(idT, childT) llc
)(DramLLC) provisos(
    Bits#(idT, a__), Bits#(childT, b__),
    Add#(SizeOf#(Line), 0, DramUserDataSz) // make sure Line sz = Dram data sz
);
    Bool verbose = True;

    // rule ordering:
    // sendResp < findResp < doDramResp < decLatency < doReq

    // FIFOs to DRAM
    FIFO#(DramUserReq) dramReqQ <- mkFIFO;
    FIFO#(DramUserData) dramRespQ <- mkFIFO;

    // FIFO of reads in DRAM
    FIFO#(DramReqIdx) pendRdQ <- mkSizedBRAMFIFO(valueof(DramLLCMaxReads));

    // MSHRs to keep in-flight read and write requests
    Vector#(DramMaxReqs, Ehr#(2, Bool))        valid      <- replicateM(mkEhr(False));
    Vector#(DramMaxReqs, Ehr#(1, idT))         reqId      <- replicateM(mkEhr(?));
    Vector#(DramMaxReqs, Ehr#(1, childT))      reqChild   <- replicateM(mkEhr(?));
    Vector#(DramMaxReqs, Ehr#(1, Bool))        reqIsLd    <- replicateM(mkEhr(?));
    Vector#(DramMaxReqs, Ehr#(2, DramLatency)) remainTime <- replicateM(mkEhr(?));
    Vector#(DramMaxReqs, Ehr#(2, Bool))        dataReady  <- replicateM(mkEhr(?));
    Vector#(DramMaxReqs, Ehr#(2, Bool))        responding <- replicateM(mkEhr(?));
    RegFile#(DramReqIdx, DramUserData)         respData   <- mkRegFile(0, fromInteger(valueof(DramMaxReqs) - 1));

    let valid_sendResp = getVEhrPort(valid, 0); // write
    let valid_findResp = getVEhrPort(valid, 1);
    let valid_dramResp = getVEhrPort(valid, 1);
    let valid_req      = getVEhrPort(valid, 1); // write

    let reqId_sendResp = getVEhrPort(reqId, 0);
    let reqId_req      = getVEhrPort(reqId, 0); // write

    let reqChild_sendResp = getVEhrPort(reqChild, 0);
    let reqChild_req      = getVEhrPort(reqChild, 0); // write

    let reqIsLd_sendResp = getVEhrPort(reqIsLd, 0);
    let reqIsLd_findResp = getVEhrPort(reqIsLd, 0);
    let reqIsLd_dramResp = getVEhrPort(reqIsLd, 0);
    let reqIsLd_req      = getVEhrPort(reqIsLd, 0); // write

    let remainTime_sendResp = getVEhrPort(remainTime, 0);
    let remainTime_findResp = getVEhrPort(remainTime, 0);
    let remainTime_decLat   = getVEhrPort(remainTime, 0); // write
    let remainTime_req      = getVEhrPort(remainTime, 1); // write

    let dataReady_sendResp = getVEhrPort(dataReady, 0);
    let dataReady_findResp = getVEhrPort(dataReady, 0);
    let dataReady_dramResp = getVEhrPort(dataReady, 0); // write
    let dataReady_req      = getVEhrPort(dataReady, 1); // write

    let responding_sendResp = getVEhrPort(responding, 0);
    let responding_findResp = getVEhrPort(responding, 0); // write
    let responding_dramResp = getVEhrPort(responding, 1); // assert
    let responding_req      = getVEhrPort(responding, 1); // write

    // FIFO of idx to resp
    FIFO#(DramReqIdx) respIdxQ <- mkFIFO;

    // Free entry Q
    FIFO#(DramReqIdx) freeQ <- mkSizedFIFO(valueof(DramMaxReqs));
    Reg#(DramReqIdx) initFreeQIdx <- mkReg(0);
    Reg#(Bool) freeQInited <- mkReg(False);

    // DRAM latncy
    Reg#(DramLatency) latency <- mkRegU;
    Reg#(Bool) latencyInited <- mkReg(False);

    Bool inited = freeQInited && latencyInited;

    rule doInitFreeQ(!freeQInited);
        freeQ.enq(initFreeQIdx);
        initFreeQIdx <= initFreeQIdx + 1;
        if(initFreeQIdx == fromInteger(valueof(DramMaxReqs) - 1)) begin
            freeQInited <= True;
        end
    endrule

    (* fire_when_enabled, no_implicit_conditions *)
    rule decLatency(inited);
        for(Integer i = 0; i < valueof(DramMaxReqs); i = i+1) begin
            DramLatency lat = remainLatency_decLat[i];
            if(lat > 0) begin
                remainLatency_decLat[i] <= lat - 1;
            end
        end
    endrule

    function DramUserAddr getDramAddrFromLLC(Addr a);
        // when requesting DRAM, we need to subtract the main mem base from the
        // requesting addr
        Addr dramBase = {mainMemBaseAddr, 3'b0};
        Addr addr = a - dramBase;
        return truncate(addr >> valueof(TLog#(DramUserBESz)));
    endfunction

    rule doReq(inited);
        // get free entry
        freeQ.deq;
        let idx = freeQ.first;
        doAssert(!valid_req[idx], "free entry must be invalid");
        // get req from LLC
        llc.toM.deq;
        let req = llc.toM.first;
        // figure out dram req, entry init values, etc.
        DramUserReq dramReq = ?;
        idT id = ?;
        childT child = ?;
        Bool isLd = False;
        case(req) matches
            tagged Ld .ld: begin
                dramReq = DramUserReq {
                    addr: getDramAddrFromLLC(ld.addr),
                    data: ?,
                    wrBE: 0
                };
                id = ld.id;
                child = ld.child;
                isLd = True;
            end
            tagged Wb .wb: begin
                dramReq = DramUserReq {
                    addr: getDramAddrFromLLC(wb.addr),
                    data: pack(wb.data),
                    wrBE: pack(wb.byteEn)
                };
                doAssert(dramReq.wrBE != 0, "St req cannot have all 0 BE");
                id = ?;
                child = ?;
                isLd = False;
            end
            default: begin
                doAssert(False, "unknown LLC req");
            end
        endcase
        // req DRAM and pend read FIFO
        dramReq.enq(dramReq);
        if(isLd) begin
            pendRdQ.enq(idx);
        end
        // set up entry
        valid_req[idx] <= True;
        reqId_req[idx] <= id;
        reqChild_req[idx] <= child;
        reqIsLd_req[idx] <= isLd;
        remainLatency_req[idx] <= latency;
        dataReady_req[idx] <= False;
        responding_req[idx] <= False;
        if(verbose) begin
            $display("[DramLLC doReq] ", fshow(req), " ; ", fshow(idx), "; ", fshow(dramReq));
        end
    endrule

    rule doDramResp(inited);
        let data <- toGet(dramRespQ).get;
        let idx <- toGet(pendRdQ).get;
        respData.upd(idx, data);
        dataReady_dramResp[idx] <= True;
        if(verbose) begin
            $display("[DramLLC doDramResp] ", fshow(idx), "; ", fshow(data));
        end
        doAssert(valid_dramResp[idx], "must be valid");
        doAssert(reqIsLd_dramResp[idx], "must be load");
        doAssert(!dataReady_dramResp[idx], "data not ready");
        doAssert(!responding_dramResp[idx], "must not be responding");
    endrule

    rule findResp(inited);
        function Bool readyToResp(DramReqIdx i);
            return valid_findResp[i] && // valid
                   remainTime_findResp[i] == 0 && // latency all elapsed
                   !responding_findResp[i] && // have not scheduled for resp
                   (reqIsLd_findResp[i] ? dataReady_findResp[i] : True); // for Ld, data is ready
        endfunction
        Vector#(DramMaxReqs, DramReqIdx) idxVec = genWith(fromInteger);
        if(find(readyToResp, idxVec) matches tagged Valid .idx) begin
            responding_findResp[idx] <= True; // record that entry has been scheduled for resp
            respIdxQ.enq(idx);
            if(verbose) begin
                $display("[DramLLC findResp] ", fshow(idx));
            end
        end
    endrule

    rule sendResp(inited);
        let idx <- toGet(respIdxQ).get;
        doAssert(valid_sendResp[idx], "must be valid");
        doAssert(responding_sendResp[idx], "must be responding");
        doAssert(remainTime_sendResp[idx] == 0, "timer must be 0");
        // send resp to LLC in case of a load
        if(reqIsLd_sendResp[idx]) begin
            MemRsMsg#(idT, childT) resp = MemRsMsg {
                data: unpack(respData.sub(idx)),
                child: reqChild_sendResp[idx],
                id: reqId_sendResp[idx]
            };
            llc.rsFromM.enq(resp);
            doAssert(dataReady_sendResp[idx], "data must be valid");
            if(verbose) begin
                $display("[DramLLC sendResp] Ld ", fshow(idx), "; ", fshow(resp));
            end
        end
        else begin
            if(verbose) begin
                $display("[DramLLC sendResp] St ", fshow(idx));
            end
        end
        // free entry
        valid_sendResp[idx] <= False;
        freeQ.enq(idx);
    endrule

    interface Client toDram;
        interface Get request = toGet(dramReqQ);
        interface Put response = toPut(dramRespQ);
    endinterface

    method Action setLatency(DramLatency x) if(!latencyInited);
        // The real latency is latency+2, so subtract 2 here
        let lat = x > 2 ? x - 2 : 0;
        latency <= lat;
        latencyInited <= True;
        if(verbose) $display("[DramLLC setLatency] %d", lat);
    endmethod
endmodule
