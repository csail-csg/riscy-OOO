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

typedef `TLB_SIZE DTlbSize;

typedef struct {
    Vpn vpn;
    TlbRqToPType t;
    Bool write; // when t = LdTranslation, whether the access on the page is a write
} DTlbRqToP deriving(Bits, Eq, FShow);

typedef struct {
    // may get page fault: i.e. hit invalid page or
    // get non-leaf page at last-level page table
    Maybe#(TlbEntry) entry; 
} DTlbTransRsFromP deriving(Bits, Eq, FShow);

interface DTlbToParent;
    interface FifoDeq#(DTlbRqToP) rqToP;
    interface FifoEnq#(DTlbTransRsFromP) ldTransRsFromP;
    interface FifoEnq#(void) setDirtyRsFromP;
    // after DTLB flush itself, it notifies L2, and wait L2 to flush
    interface Client#(void, void) flush;
endinterface

interface DTlb;
    // system consistency related
    method Bool noPendingReqOrWrite;
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
    Reg#(VMInfo) vm_info <- mkReg(VMInfo{prv:2'b11, asid:0, vm:0, base:0, bound:-1});

    // blocking miss
    Reg#(Maybe#(TlbReq)) miss <- mkReg(Invalid);

    // req & resp with parent TLB
    Fifo#(2, DTlbRqToP) rqToPQ <- mkCFFifo;
    Fifo#(2, DTlbTransRsFromP) ldTransRsFromPQ <- mkCFFifo;
    Fifo#(2, void) setDirtyRsFromPQ <- mkCFFifo;
    // flush req/resp with parent TLB
    Fifo#(1, void) flushRqToPQ <- mkCFFifo;
    Fifo#(1, void) flushRsFromPQ <- mkCFFifo;

    // FIFO for perf req
    Fifo#(1, TlbPerfType) perfReqQ <- mkCFFifo;

    // counter for pending writes (set dirty)
    SafeCounter#(Bit#(4)) pendSetDirtyCnt <- mkSafeCounter(0);

    // do flush: start when all misses resolve & no pending write
    rule doStartFlush(needFlush && !waitFlushP && !isValid(miss) && pendSetDirtyCnt == 0);
        tlb.flush_translation(unpack(0), unpack(0), True);
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

    function Bool hasPermission(PTE_Type page_perm, Bool write);
        return hasSv39Permission(vm_info, False, write, page_perm);
    endfunction

    rule doTransPRs(miss matches tagged Valid .r);
        ldTransRsFromPQ.deq;
        let pRs = ldTransRsFromPQ.first;

        if(pRs.entry matches tagged Valid .en) begin
            // TODO when we have multiple misses in future
            // we first need to search TLB to check whether the PTE is already in TLB
            // this may happen for mega/giga pages
            // we don't want same PTE to occupy >1 TLB entires
            
            // check permission
            if(hasPermission(en.page_perm, r.write)) begin
                // access (& dirty) bit already set in parent TLB, just fill TLB and resp to proc
                tlb.add_translation(en);
                let trans_addr = translate(r.addr, en.ppn, en.page_size);
                hitQ.enq(tuple2(trans_addr, Invalid));
                if(verbose) $display("DTLB %m refill: ", fshow(r), " ; ", fshow(trans_addr));
            end
            else begin
                // Don't have needed permission for this page
                hitQ.enq(tuple2(?, Valid (r.write ? StoreAccessFault : LoadAccessFault)));
                if(verbose) $display("DTLB %m refill no permission: ", fshow(r));
            end
        end
        else begin
            // page fault
            hitQ.enq(tuple2(?, Valid (r.write ? StoreAccessFault : LoadAccessFault)));
            if(verbose) $display("DTLB %m refill page fault: ", fshow(r));
        end
        // miss resolved
        miss <= Invalid;
    endrule

    rule doSetDirtyPRs;
        setDirtyRsFromPQ.deq;
        pendSetDirtyCnt.decr(1);
    endrule


    method Bool noPendingReqOrWrite;
        return !isValid(miss) && pendSetDirtyCnt == 0;
    endmethod

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

    // we do not accept new req when flushing flag is set
    // we also make the guard more restrictive to reduce the time of computing guard
    // i.e. guard does not depend on whether TLB hit or miss
    method Action procReq(TlbReq r) if(!needFlush && !isValid(miss) && hitQ.notFull && rqToPQ.notFull);
        if (vm_info.vm == vmSv39) begin
            let vpn = getVpn(r.addr);
            let trans_result = tlb.translate(vpn, vm_info.asid);
            if (trans_result.hit) begin
                // TLB hit
                let entry = trans_result.entry;
                // check permission
                if (hasPermission(entry.page_perm, r.write)) begin
                    // update dirty bit
                    Bool first_set_dirty = !entry.dirty && r.write;
                    entry.dirty = entry.dirty || r.write;
                    tlb.update(trans_result.index, r.write);
                    // translate addr
                    Addr trans_addr = translate(r.addr, entry.ppn, entry.page_size);
                    hitQ.enq(tuple2(trans_addr, Invalid));
                    // if first time update dirty bit, write through to parent TLB
                    if(first_set_dirty) begin
                        rqToPQ.enq(DTlbRqToP {
                            vpn: vpn,
                            t: SetDirtyOnly,
                            write: True
                        });
                        pendSetDirtyCnt.incr(1);
                    end
                    if(verbose) $display("DTLB %m req (hit): ", fshow(r), " ; ", fshow(trans_result));
                end
                else begin
                    // Don't have needed permission for this page
                    hitQ.enq(tuple2(?, Valid (r.write ? StoreAccessFault : LoadAccessFault)));
                    if(verbose) $display("DTLB %m req no permission: ", fshow(r));
                end
            end
            else begin
                // TLB miss, req to parent TLB
                miss <= Valid (r);
                rqToPQ.enq(DTlbRqToP {
                    vpn: vpn,
                    t: LdTranslation,
                    write: r.write
                });
                if(verbose) $display("DTLB %m req (miss): ", fshow(r));
            end
        end
        else if (vm_info.vm == vmMbare || vm_info.vm == vmMbb || vm_info.vm == vmMbbid) begin
            if(verbose) $display("TLB %m req (untrans): ", fshow(r));
            if (r.addr > vm_info.bound - vm_info.base) begin
                hitQ.enq(tuple2(?, Valid (r.write ? StoreAccessFault : LoadAccessFault)));
            end
            else begin
                let paddr = r.addr + vm_info.base;
                hitQ.enq(tuple2(paddr, Invalid));
            end
        end
        else begin
            // Illegal paging mode
            doAssert(False, "Unsupported paging mode");
            hitQ.enq(tuple2(?, Valid (r.write ? StoreAccessFault : LoadAccessFault)));
        end
    endmethod

    method Action deqProcResp;
        hitQ.deq;
    endmethod

    method procResp = hitQ.first;

    interface DTlbToParent toParent;
        interface rqToP = toFifoDeq(rqToPQ);
        interface ldTransRsFromP = toFifoEnq(ldTransRsFromPQ);
        interface setDirtyRsFromP = toFifoEnq(setDirtyRsFromPQ);
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
