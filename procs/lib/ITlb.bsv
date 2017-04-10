`include "ProcConfig.bsv"
import ClientServer::*;
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

// currently blocking
typedef `TLB_SIZE ITlbSize;

typedef struct {
    Vpn vpn;
} ITlbRqToP deriving(Bits, Eq, FShow);

typedef struct {
    // may get page fault: i.e. hit invalid page or
    // get non-leaf page at last-level page table
    Maybe#(TlbEntry) entry; 
} ITlbRsFromP deriving(Bits, Eq, FShow);

interface ITlbToParent;
    interface FifoDeq#(ITlbRqToP) rqToP;
    interface FifoEnq#(ITlbRsFromP) rsFromP;
    // after ITLB flush itself, it notifies L2, and wait L2 to flush
    interface Client#(void, void) flush;
endinterface

interface ITlb;
    // system consistency related
    method Bool noPendingReq;
    method Bool flush_done;
    method Action flush;
    method Action updateVMInfo(VMInfo vm);

    // req/resp with core
    interface Server#(Addr, TlbResp) to_proc;

    // req/resp with L2 TLB
    interface ITlbToParent toParent;

    // performance
    interface Perf#(TlbPerfType) perf;
endinterface

typedef FullAssocTlb#(ITlbSize) ITlbArray;
module mkITlbArray(ITlbArray);
    let m <- mkFullAssocTlb;
    return m;
endmodule

(* synthesize *)
module mkITlb(ITlb::ITlb);
    Bool verbose = True;

    // TLB array
    ITlbArray tlb <- mkITlbArray;

    // processor init flushing by setting this flag
    Reg#(Bool) needFlush <- mkReg(False);
    // after flushing ITLB itself, we want parent TLB to flush
    Reg#(Bool) waitFlushP <- mkReg(False);

    // resp FIFO to proc
    Fifo#(2, TlbResp) hitQ <- mkCFFifo;

    // current processor VM information
    Reg#(VMInfo) vm_info <- mkReg(VMInfo{prv:2'b11, asid:0, vm:0, base:0, bound:-1});

    // blocking miss
    Reg#(Maybe#(Addr)) miss <- mkReg(Invalid);

    // req & resp with parent TLB
    Fifo#(2, ITlbRqToP) rqToPQ <- mkCFFifo;
    Fifo#(2, ITlbRsFromP) rsFromPQ <- mkCFFifo;
    // flush req/resp with parent TLB
    Fifo#(1, void) flushRqToPQ <- mkCFFifo;
    Fifo#(1, void) flushRsFromPQ <- mkCFFifo;

    // FIFO for perf req
    Fifo#(1, TlbPerfType) perfReqQ <- mkCFFifo;

    // do flush: only start when all misses resolve
    rule doStartFlush(needFlush && !waitFlushP && !isValid(miss));
        tlb.flush_translation(unpack(0), unpack(0), True);
        // request parent TLB to flush
        flushRqToPQ.enq(?);
        waitFlushP <= True;
        if(verbose) $display("ITLB %m: flush begin");
    endrule

    rule doFinishFlush(needFlush && waitFlushP && !isValid(miss));
        flushRsFromPQ.deq;
        needFlush <= False;
        waitFlushP <= False;
        if(verbose) $display("ITLB %m: flush done");
    endrule

    function Bool hasPermission(PTE_Type page_perm);
        return hasSv39Permission(vm_info, True, False, page_perm);
    endfunction

    rule doRsFromP(miss matches tagged Valid .vaddr);
        rsFromPQ.deq;
        let pRs = rsFromPQ.first;

        if(pRs.entry matches tagged Valid .en) begin
            // TODO when we have multiple misses in future
            // we first need to search TLB to check whether the PTE is already in TLB
            // this may happen for mega/giga pages
            // we don't want same PTE to occupy >1 TLB entires
            
            // check permission
            if(hasPermission(en.page_perm)) begin
                // access bit already set in parent TLB, just fill TLB and resp to proc
                tlb.add_translation(en);
                let trans_addr = translate(vaddr, en.ppn, en.page_size);
                hitQ.enq(tuple2(trans_addr, Invalid));
                if(verbose) $display("ITLB %m refill: ", fshow(vaddr), " ; ", fshow(trans_addr));
            end
            else begin
                // Don't have needed permission for this page
                hitQ.enq(tuple2(?, Valid (InstAccessFault)));
                if(verbose) $display("ITLB %m refill no permission: ", fshow(vaddr));
            end
        end
        else begin
            // page fault
            hitQ.enq(tuple2(?, Valid (InstAccessFault)));
            if(verbose) $display("ITLB %m refill page fault: ", fshow(vaddr));
        end
        // miss resolved
        miss <= Invalid;
    endrule

    // wire for no pending req, solve schedule conflict when ITLB is put inside fetchStage
    Wire#(Bool) no_pending_wire <- mkBypassWire;
    (* fire_when_enabled, no_implicit_conditions *)
    rule setNoPendingWire;
        no_pending_wire <= !isValid(miss);
    endrule

    method Bool noPendingReq = no_pending_wire;

    method Action flush if(!needFlush);
        needFlush <= True;
        waitFlushP <= False;
        // this won't interrupt current processing, since
        // (1) miss process will continue even if inited=True
        // (2) flush truly starts when there is no pending req
    endmethod

    method Bool flush_done = !needFlush;

    method Action updateVMInfo(VMInfo vm);
        vm_info <= vm;
    endmethod

    interface Server to_proc;
        interface Put request;
            // we do not accept new req when flushing flag is set
            // we also make the guard more restrictive to reduce the time of computing guard
            // i.e. guard does not depend on whether TLB hit or miss
            method Action put(Addr vaddr) if(!needFlush && !isValid(miss) && hitQ.notFull && rqToPQ.notFull);
                if (vm_info.vm == vmSv39) begin
                    let vpn = getVpn(vaddr);
                    let trans_result = tlb.translate(vpn, vm_info.asid);
                    if (trans_result.hit) begin
                        // TLB hit
                        let entry = trans_result.entry;
                        // check permission
                        if (hasPermission(entry.page_perm)) begin
                            // update LRU in TLB, I TLB never writes
                            tlb.update(trans_result.index, False);
                            // translate addr
                            Addr trans_addr = translate(vaddr, entry.ppn, entry.page_size);
                            hitQ.enq(tuple2(trans_addr, Invalid));
                            if(verbose) $display("ITLB %m req (hit): ", fshow(vaddr), " ; ", fshow(trans_result));
                        end
                        else begin
                            // Don't have needed permission for this page
                            hitQ.enq(tuple2(?, Valid (InstAccessFault)));
                            if(verbose) $display("ITLB %m req no permission: ", fshow(vaddr));
                        end
                    end
                    else begin
                        // TLB miss, req to parent TLB
                        miss <= Valid (vaddr);
                        rqToPQ.enq(ITlbRqToP {
                            vpn: vpn
                        });
                        if(verbose) $display("ITLB %m req (miss): ", fshow(vaddr));
                    end
                end
                else if (vm_info.vm == vmMbare || vm_info.vm == vmMbb || vm_info.vm == vmMbbid) begin
                    if (verbose) $display("ITLB %m req (untrans): ", fshow(vaddr));
                    if (vaddr > vm_info.bound - vm_info.base) begin
                        hitQ.enq(tuple2(?, Valid (InstAccessFault)));
                    end
                    else begin
                        let paddr = vaddr + vm_info.base;
                        hitQ.enq(tuple2(paddr, Invalid));
                    end
                end
                else begin
                    // Illegal paging mode
                    doAssert(False, "Unsupported paging mode");
                    hitQ.enq(tuple2(?, Valid (InstAccessFault)));
                end
            endmethod
        endinterface
        interface Get response = toGet(hitQ);
    endinterface

    interface ITlbToParent toParent;
        interface rqToP = toFifoDeq(rqToPQ);
        interface rsFromP = toFifoEnq(rsFromPQ);
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
