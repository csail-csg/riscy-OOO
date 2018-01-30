
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

`include "ProcConfig.bsv"
import ClientServer::*;
import DefaultValue::*;
import GetPut::*;
import Types::*;
import ProcTypes::*;
import TlbTypes::*;
import Performance::*;
import FullAssocTlb::*;
import ConfigReg::*;
import Fifo::*;
import Cntrs::*;
import SafeCounter::*;
import CacheUtils::*;

typedef `TLB_SIZE DTlbSize;

typedef struct {
    Vpn vpn;
} DTlbRqToP deriving(Bits, Eq, FShow);

typedef struct {
    // may get page fault: i.e. hit invalid page or
    // get non-leaf page at last-level page table
    Maybe#(TlbEntry) entry; 
} DTlbTransRsFromP deriving(Bits, Eq, FShow);

interface DTlbToParent;
    interface FifoDeq#(DTlbRqToP) rqToP;
    interface FifoEnq#(DTlbTransRsFromP) ldTransRsFromP;
    // after DTLB flush itself, it notifies L2, and wait L2 to flush
    interface Client#(void, void) flush;
endinterface

interface DTlb;
    // system consistency related
    method Bool flush_done;
    method Action flush;
    method Action updateVMInfo(VMInfo vm);

    // req/resp with core
    method Action procReq(TlbReq r);
    method TlbResp procResp;
    method Action deqProcResp;

    // req/resp with L2 TLB
    interface DTlbToParent toParent;

    // performance
    interface Perf#(TlbPerfType) perf;
endinterface

typedef FullAssocTlb#(DTlbSize) DTlbArray;
module mkDTlbArray(DTlbArray);
    let m <- mkFullAssocTlb;
    return m;
endmodule

(* synthesize *)
module mkDTlb(DTlb::DTlb);
    Bool verbose = True;

    // TLB array
    DTlbArray tlb <- mkDTlbArray;

    // processor init flushing by setting this flag
    Reg#(Bool) needFlush <- mkReg(False);
    // after flushing ITLB itself, we want parent TLB to flush
    Reg#(Bool) waitFlushP <- mkReg(False);

    // resp FIFO to proc
    Fifo#(2, TlbResp) hitQ <- mkCFFifo;

    // current processor VM information
    Reg#(VMInfo) vm_info <- mkReg(defaultValue);

    // blocking miss
    Reg#(Maybe#(TlbReq)) miss <- mkReg(Invalid);

    // req & resp with parent TLB
    Fifo#(2, DTlbRqToP) rqToPQ <- mkCFFifo;
    Fifo#(2, DTlbTransRsFromP) ldTransRsFromPQ <- mkCFFifo;
    // flush req/resp with parent TLB
    Fifo#(1, void) flushRqToPQ <- mkCFFifo;
    Fifo#(1, void) flushRsFromPQ <- mkCFFifo;

    // FIFO for perf req
    Fifo#(1, TlbPerfType) perfReqQ <- mkCFFifo;

    // do flush: start when all misses resolve & no pending write
    rule doStartFlush(needFlush && !waitFlushP && !isValid(miss));
        tlb.flush;
        // request parent TLB to flush
        flushRqToPQ.enq(?);
        waitFlushP <= True;
        if(verbose) $display("DTLB %m: flush begin");
    endrule

    rule doFinishFlush(needFlush && waitFlushP && !isValid(miss));
        flushRsFromPQ.deq;
        needFlush <= False;
        waitFlushP <= False;
        if(verbose) $display("DTLB %m: flush done");
    endrule

    rule doTransPRs(miss matches tagged Valid .r);
        ldTransRsFromPQ.deq;
        let pRs = ldTransRsFromPQ.first;

        if(pRs.entry matches tagged Valid .en) begin
            // TODO When we have multiple misses in future, we first need to
            // search TLB to check whether the PTE is already in TLB.  This may
            // happen for mega/giga pages.  We don't want same PTE to occupy >1
            // TLB entires.
            
            // check permission
            if(hasVMPermission(vm_info,
                               en.pteType,
                               en.ppn,
                               en.level,
                               r.write ? DataStore : DataLoad)) begin
                // fill TLB
                tlb.addEntry(en);
                let trans_addr = translate(r.addr, en.ppn, en.level);
                hitQ.enq(tuple2(trans_addr, Invalid));
                if(verbose) begin
                    $display("DTLB %m refill: ", fshow(r),
                             " ; ", fshow(trans_addr));
                end
            end
            else begin
                // page fault
                hitQ.enq(tuple2(
                    ?, Valid (r.write ? StorePageFault : LoadPageFault)
                ));
                if(verbose) begin
                    $display("DTLB %m refill no permission: ", fshow(r));
                end
            end
        end
        else begin
            // page fault
            hitQ.enq(tuple2(
                ?, Valid (r.write ? StorePageFault : LoadPageFault)
            ));
            if(verbose) $display("DTLB %m refill page fault: ", fshow(r));
        end
        // miss resolved
        miss <= Invalid;
    endrule

    method Action flush if(!needFlush);
        needFlush <= True;
        waitFlushP <= False;
        // this won't interrupt current processing, since
        // (1) miss process will continue even if needFlush=True
        // (2) flush truly starts when there is no pending req
    endmethod

    method Bool flush_done = !needFlush;

    method Action updateVMInfo(VMInfo vm) if(!isValid(miss));
        vm_info <= vm;
    endmethod

    // We do not accept new req when flushing flag is set. We also make the
    // guard more restrictive to reduce the time of computing guard, i.e. guard
    // does not depend on whether TLB hit or miss.
    method Action procReq(TlbReq r) if(
        !needFlush && !isValid(miss) && hitQ.notFull && rqToPQ.notFull
    );
        if (vm_info.sv39) begin
            let vpn = getVpn(r.addr);
            let trans_result = tlb.translate(vpn, vm_info.asid);
            if (trans_result.hit) begin
                // TLB hit
                let entry = trans_result.entry;
                // check permission
                if (hasVMPermission(vm_info,
                                    entry.pteType,
                                    entry.ppn,
                                    entry.level,
                                    r.write ? DataStore : DataLoad)) begin
                    // update TLB replacement info
                    tlb.updateRep(trans_result.index);
                    // translate addr
                    Addr trans_addr = translate(r.addr, entry.ppn, entry.level);
                    hitQ.enq(tuple2(trans_addr, Invalid));
                    if(verbose) begin
                        $display("DTLB %m req (hit): ", fshow(r),
                                 " ; ", fshow(trans_result));
                    end
                end
                else begin
                    // page fault
                    hitQ.enq(tuple2(
                        ?, Valid (r.write ? StorePageFault : LoadPageFault)
                    ));
                    if(verbose) $display("DTLB %m req no permission: ", fshow(r));
                end
            end
            else begin
                // TLB miss, req to parent TLB
                miss <= Valid (r);
                rqToPQ.enq(DTlbRqToP {
                    vpn: vpn
                });
                if(verbose) $display("DTLB %m req (miss): ", fshow(r));
            end
        end
        else begin
            // bare mode
            hitQ.enq(tuple2(r.addr, Invalid));
            if(verbose) $display("DTLB %m req (bare): ", fshow(r));
        end
    endmethod

    method Action deqProcResp;
        hitQ.deq;
    endmethod

    method procResp = hitQ.first;

    interface DTlbToParent toParent;
        interface rqToP = toFifoDeq(rqToPQ);
        interface ldTransRsFromP = toFifoEnq(ldTransRsFromPQ);
        interface Client flush;
            interface request = toGet(flushRqToPQ);
            interface response = toPut(flushRsFromPQ);
        endinterface
    endinterface

    interface Perf perf;
        method Action setStatus(Bool stats);
            noAction;
        endmethod

        method Action req(TlbPerfType r);
            perfReqQ.enq(r);
        endmethod

        method ActionValue#(PerfResp#(TlbPerfType)) resp;
            perfReqQ.deq;
            return PerfResp {
                pType: perfReqQ.first,
                data: 0
            };
        endmethod

        method Bool respValid = perfReqQ.notEmpty;
    endinterface
endmodule
