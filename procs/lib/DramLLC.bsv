
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
import Vector::*;
import RegFile::*;

import Ehr::*;
import Types::*;
import ProcTypes::*;
import CacheUtils::*;
import CCTypes::*;
import DramCommon::*;
import MMIOAddrs::*;
import LLCache::*;

// XXX Processor may fetch out-of-range addresses into cache. Normally, these
// data should be in S state, and should never be written back to memory.
// However, since the current cache does not have E state, for single-core
// efficiency, the processor may acquire M state even for a load. This will
// lead to writebacks of these out-of-range addresses. We have to prevent these
// writebacks from overwriting memory values. Therefore, when receiving a
// out-of-range req, we never send it to DRAM, but mark it as done directly.

export mkDramLLC;
export DramLLC(..);

typedef Bit#(TLog#(DramMaxReqs)) DramReqIdx;
typedef Bit#(TLog#(DramLatency)) DramTimer;

interface DramLLC;
    interface Client#(DramUserReq, DramUserData) toDram;
    method Action start(Addr addrOverflowMask);
endinterface

module mkDramLLC#(MemFifoClient#(idT, childT) llc)(DramLLC) provisos(
    Bits#(idT, a__), Bits#(childT, b__),
    FShow#(ToMemMsg#(idT, childT)), FShow#(MemRsMsg#(idT, childT)),
    Add#(SizeOf#(Line), 0, DramUserDataSz) // make sure Line sz = Dram data sz
);
    Bool verbose = True;

    // rule ordering:
    // sendResp < findResp < doDramResp < decLatency < doReq

    // FIFOs to DRAM
    FIFO#(DramUserReq) dramReqQ <- mkFIFO;
    FIFO#(DramUserData) dramRespQ <- mkFIFO;

    // FIFO of reads in DRAM
    FIFO#(DramReqIdx) pendRdQ <- mkSizedFIFO(valueof(DramMaxReads));

    // addr overflow mask (set once at init time)
    Reg#(Addr) addrOverflowMask <- mkReg(0);
    Reg#(Bool) maskSet <- mkReg(False);

    // MSHRs to keep in-flight read and write requests
    Vector#(DramMaxReqs, Ehr#(2, Bool))      valid        <- replicateM(mkEhr(False));
    Vector#(DramMaxReqs, Ehr#(1, idT))       reqId        <- replicateM(mkEhr(?));
    Vector#(DramMaxReqs, Ehr#(1, childT))    reqChild     <- replicateM(mkEhr(?));
    Vector#(DramMaxReqs, Ehr#(1, Bool))      reqIsLd      <- replicateM(mkEhr(?));
    Vector#(DramMaxReqs, Ehr#(1, Bool))      addrOverflow <- replicateM(mkEhr(?));
    Vector#(DramMaxReqs, Ehr#(2, DramTimer)) remainTime   <- replicateM(mkEhr(?));
    Vector#(DramMaxReqs, Ehr#(2, Bool))      dataReady    <- replicateM(mkEhr(?));
    Vector#(DramMaxReqs, Ehr#(2, Bool))      responding   <- replicateM(mkEhr(?));
    RegFile#(DramReqIdx, DramUserData)       respData     <- mkRegFile(0, fromInteger(valueof(DramMaxReqs) - 1));

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

    let addrOverflow_sendResp = getVEhrPort(addrOverflow, 0);
    let addrOverflow_req      = getVEhrPort(addrOverflow, 0); // write

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

    // DRAM latncy: the real latency is latency+2, so subtract 2 here
    DramTimer latency = fromInteger(valueof(DramLatency) - 2);

    // Don't accept req until freeQ and addrOverflow mask are initialized
    Bool inited = freeQInited && maskSet;

    rule doInitFreeQ(!freeQInited);
        freeQ.enq(initFreeQIdx);
        initFreeQIdx <= initFreeQIdx + 1;
        if(initFreeQIdx == fromInteger(valueof(DramMaxReqs) - 1)) begin
            freeQInited <= True;
            if(verbose) $display("[DRAMLLC doInitFreeQ] freeQ init done");
        end
    endrule

    (* fire_when_enabled, no_implicit_conditions *)
    rule decLatency(inited);
        for(Integer i = 0; i < valueof(DramMaxReqs); i = i+1) begin
            DramTimer lat = remainTime_decLat[i];
            if(lat > 0) begin
                remainTime_decLat[i] <= lat - 1;
            end
        end
    endrule

    // return DRAM addr and whether addr overflow
    function Tuple2#(DramUserAddr, Bool) getDramAddrFromLLC(Addr a);
        // when requesting DRAM, we need to subtract the main mem base from the
        // requesting addr
        Addr dramBase = {mainMemBaseAddr, 3'b0};
        Addr addr = a - dramBase;
        Bool overflow = (addr & addrOverflowMask) != 0;
        DramUserAddr dramAddr = truncate(addr >> valueof(TLog#(DramUserBESz)));
        return tuple2(dramAddr, overflow);
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
        Bool addr_overflow = False;
        case(req) matches
            tagged Ld .ld: begin
                let {addr, overflow} = getDramAddrFromLLC(ld.addr);
                dramReq = DramUserReq {
                    addr: addr,
                    data: ?,
                    wrBE: 0
                };
                id = ld.id;
                child = ld.child;
                isLd = True;
                addr_overflow = overflow;
            end
            tagged Wb .wb: begin
                let {addr, overflow} = getDramAddrFromLLC(wb.addr);
                dramReq = DramUserReq {
                    addr: addr,
                    data: pack(wb.data),
                    wrBE: pack(wb.byteEn)
                };
                doAssert(dramReq.wrBE != 0, "St req cannot have all 0 BE");
                id = ?;
                child = ?;
                isLd = False;
                addr_overflow = overflow;
            end
            default: begin
                doAssert(False, "unknown LLC req");
            end
        endcase

        if(addr_overflow) begin
            // we mark this entry as 0 latency and data ready (in case of
            // load). (We don't discard the data or set the responding bit,
            // because we need to go through other rules to recycle the entry
            // and send load response.)
            valid_req[idx] <= True;
            reqId_req[idx] <= id;
            reqChild_req[idx] <= child;
            reqIsLd_req[idx] <= isLd;
            addrOverflow_req[idx] <= True;
            remainTime_req[idx] <= 0;
            dataReady_req[idx] <= isLd; // load will have a dummy data
            responding_req[idx] <= False;
            if(verbose) begin
                $display("[DramLLC doReq] mark overflow req as responding: ",
                         fshow(req), " ; ", fshow(idx));
            end
        end
        else begin
            // valid addr, req DRAM and pend read FIFO
            dramReqQ.enq(dramReq);
            if(isLd) begin
                pendRdQ.enq(idx);
            end
            // set up entry
            valid_req[idx] <= True;
            reqId_req[idx] <= id;
            reqChild_req[idx] <= child;
            reqIsLd_req[idx] <= isLd;
            addrOverflow_req[idx] <= False;
            remainTime_req[idx] <= latency;
            dataReady_req[idx] <= False;
            responding_req[idx] <= False;
            if(verbose) begin
                $display("[DramLLC doReq] send DRAM req: ",
                         fshow(req), " ; ", fshow(idx), "; ", fshow(dramReq));
            end
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
                // For security, let's give a fixed value if address is
                // overflowed
                data: addrOverflow_sendResp[idx] ? unpack(0) : unpack(respData.sub(idx)),
                child: reqChild_sendResp[idx],
                id: reqId_sendResp[idx]
            };
            llc.rsFromM.enq(resp);
            doAssert(dataReady_sendResp[idx], "data must be valid");
            if(verbose) begin
                $display("[DramLLC sendResp] Ld ", fshow(idx), "; ",
                         fshow(addrOverflow_sendResp[idx]), "; ", fshow(resp));
            end
        end
        else begin
            if(verbose) begin
                $display("[DramLLC sendResp] St ", fshow(idx), "; ",
                         fshow(addrOverflow_sendResp[idx]));
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
    
    method Action start(Addr mask) if(!maskSet);
        addrOverflowMask <= mask;
        maskSet <= True;
    endmethod
endmodule
