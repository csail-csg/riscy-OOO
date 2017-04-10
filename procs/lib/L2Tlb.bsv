`include "ProcConfig.bsv"
import Vector::*;
import ClientServer::*;
import GetPut::*;
import Types::*;
import ProcTypes::*;
import TlbTypes::*;
import Performance::*;
import FullAssocTlb::*;
import ConfigReg::*;
import Ehr::*;
import Fifo::*;
import Cntrs::*;
import SafeCounter::*;
import CacheUtils::*;
import SetAssocTlb::*;
import L2SetAssocTlb::*;

// for SV39 only

// curretly L2 TLB is just a blocking page walker

// interface with memory (LLC)
typedef enum {Null} TlbMemReqId deriving(Bits, Eq, FShow);

typedef struct {
    Bool write;
    Addr addr;
    Data data;
    TlbMemReqId id;
} TlbMemReq deriving(Bits, Eq, FShow);

typedef struct {
    Data data;
    TlbMemReqId id;
} TlbLdResp deriving(Bits, Eq, FShow);

interface TlbMemClient;
    interface FifoDeq#(TlbMemReq) memReq;
    interface FifoEnq#(TlbLdResp) respLd;
    interface FifoEnq#(TlbMemReqId) respSt;
endinterface

// interface with children (I/D TLB)
typedef enum {I, D} TlbChild deriving(Bits, Eq, FShow);
typedef struct {
    TlbChild child;
    Vpn vpn;
    TlbRqToPType reqType; // DTLB req type; ITLB should set to LdTranslation
    Bool write; // DTLB write or not; ITLB should set to False
} L2TlbRqFromC deriving(Bits, Eq, FShow);

typedef struct {
    TlbChild child;
    Maybe#(TlbEntry) entry;
} L2TlbRsToC deriving(Bits, Eq, FShow);

interface L2TlbToChildren;
    interface Put#(L2TlbRqFromC) rqFromC;
    interface FifoDeq#(L2TlbRsToC) rsToC;
    interface FifoDeq#(void) setDirtyRs;
    // flush with I/D TLB
    interface Put#(void) iTlbReqFlush;
    interface Put#(void) dTlbReqFlush;
    interface Get#(void) flushDone;
endinterface

interface L2Tlb;
    // system consistency related (flush is requested by both I & D TLB)
    method Bool noPendingReqOrWrite;
    method Action updateVMInfo(VMInfo vmI, VMInfo vmD);

    // ifc with ITLb & DTLB
    interface L2TlbToChildren toChildren;

    // ifc with memory (LLC)
    interface TlbMemClient toMem;

    // performace
    interface Perf#(TlbPerfType) perf;
endinterface

typedef FullAssocTlb#(8) L2FullAssocTlb;
module mkL2FullAssocTlb(L2FullAssocTlb);
    let m <- mkFullAssocTlb;
    return m;
endmodule

(* synthesize *)
module mkL2Tlb(L2Tlb);
    Bool verbose = True;
   
    // set associative TLB for 4KB pages
    L2SetAssocTlb tlb4KB <- mkL2SetAssocTlb;
    // fully associative TLB for mega and giga pages
    L2FullAssocTlb tlbMG <- mkL2FullAssocTlb;

    // flush
    Reg#(Bool) iFlushReq <- mkReg(False);
    Reg#(Bool) dFlushReq <- mkReg(False);
    Reg#(Bool) waitFlushDone <- mkReg(False);
    Bool flushing = iFlushReq && dFlushReq;
    Fifo#(1, void) flushDoneQ <- mkCFFifo;

    // req/resp with I/D TLBs
    Fifo#(1, L2TlbRqFromC) rqFromCQ <- mkBypassFifo;
    Fifo#(1, L2TlbRsToC) rsToCQ <- mkBypassFifo;
    Fifo#(1, void) setDirtyRsQ <- mkBypassFifo;

    // pending req in set assoc TLB pipeline
    Ehr#(2, Maybe#(L2TlbRqFromC)) pendReq_ehr <- mkEhr(Invalid);
    Reg#(Maybe#(L2TlbRqFromC)) pendReq = pendReq_ehr[0];
    Reg#(Maybe#(L2TlbRqFromC)) pendReq_enq = pendReq_ehr[1];

    // page walk currently being prcessed
    Reg#(Bool) miss <- mkReg(False);
    Reg#(Bit#(2)) walkLevel <- mkRegU; // "i" in riscv spec's page walk algorithm
    Reg#(Addr) ptBaseAddr <- mkRegU; // page table base addr ("a" in spec's algorithm)
    
    // current processor VM information
    Reg#(VMInfo) vm_info_I <- mkReg(VMInfo{prv:2'b11, asid:0, vm:0, base:0, bound:-1});
    Reg#(VMInfo) vm_info_D <- mkReg(VMInfo{prv:2'b11, asid:0, vm:0, base:0, bound:-1});

    // Memory Queues for page table walks
    Fifo#(2, TlbMemReq) memReqQ <- mkCFFifo;
    Fifo#(2, TlbLdResp) respLdQ <- mkCFFifo;
    Fifo#(2, TlbMemReqId) respStQ <- mkCFFifo;

    // counter for pending stores to memory (LLC)
    SafeCounter#(Bit#(4)) pendStCnt <- mkSafeCounter(0);

    // FIFO for perf req
    Fifo#(1, TlbPerfType) perfReqQ <- mkCFFifo;

    // when flushing is true, all I/D TLB req have been responded
    // so there cannot be any req in rqFromCQ or any new req
    // The only possible pendReq is SetDirtyOnly
    rule doStartFlush(flushing && !waitFlushDone && !isValid(pendReq) && pendStCnt == 0);
        waitFlushDone <= True;
        tlb4KB.flush;
        tlbMG.flush_translation(unpack(0), unpack(0), True);
    endrule

    rule doWaitFlush(flushing && waitFlushDone && tlb4KB.flush_done);
        waitFlushDone <= False;
        flushDoneQ.enq(?);
        iFlushReq <= False;
        dFlushReq <= False;
    endrule

    // decr pend st cnt by recv st resp
    rule doStResp;
        respStQ.deq;
        pendStCnt.decr(1);
    endrule

    function Addr getPTEAddr(Addr baseAddr, Vpn vpn, Bit#(2) i);
        Vector#(3, VpnI) vpnVec = unpack(vpn); // index 0 is LSB
        return baseAddr + (zeroExtend(vpnVec[i]) << 3); // PTE is 2^3 bytes
    endfunction

    rule doTlbReq(!isValid(pendReq_enq));
        rqFromCQ.deq;
        let r = rqFromCQ.first;
        // req tlb array
        tlb4KB.req(SetAssocTlbReq {vpn: r.vpn});
        // record req
        pendReq_enq <= Valid (r);
        // directly resp SetDirtyOnly
        if(r.reqType == SetDirtyOnly) begin
            setDirtyRsQ.enq(?);
        end
        if(verbose) $display("L2TLB new req: ", fshow(r));
    endrule

    // process resp from 4KB TLB and mega-giga TLB
    rule doTlbResp(pendReq matches tagged Valid .cRq &&& !miss);
        // handle page fault
        function Action pageFault(String reason);
        action
            // page fault, just resp child with invalid entry
            // set dirty req should not incr page fault
            // because it is a hit in DTLB
            if(cRq.reqType == SetDirtyOnly) begin
                doAssert(False, reason + " should not happen for set dirty in doTlbResp");
                // resp has already been sent before
            end
            else begin
                rsToCQ.enq(L2TlbRsToC {
                    child: cRq.child,
                    entry: Invalid
                });
            end
            // req is done (we are not in miss)
            pendReq <= Invalid;
            // 4KB TLB array is not deq yet
            tlb4KB.deqUpdate(False, ?, ?);
        endaction
        endfunction

        // get correct VM info
        VMInfo vm_info = cRq.child == I ? vm_info_I : vm_info_D;

        if(vm_info.vm == vmSv39) begin
            // get resp from 4KB TLB and mega-giga TLB
            let resp4KB = tlb4KB.resp;
            let respMG = tlbMG.translate(cRq.vpn, vm_info.asid);

            if(verbose) begin
                $display("L2TLB resp: ", fshow(vm_info), " ; ", fshow(cRq), " ; ", 
                         fshow(resp4KB), " ; ", fshow(respMG));
            end

            Bool startPageWalk = False; // flag for starting page walk

            // when page hit, resp to child; return whether to start page walk for set dirty
            function ActionValue#(Bool) pageHit(TlbEntry entry, Bool first_set_dirty, String debugStr);
            actionvalue
                if(cRq.reqType == SetDirtyOnly) begin
                    // must have first set dirty, do page walk
                    doAssert(first_set_dirty, "dirty bit cannot be already set in " + debugStr);
                    return True;
                end
                else begin
                    // resp to child
                    rsToCQ.enq(L2TlbRsToC {
                        child: cRq.child,
                        entry: Valid (entry)
                    });
                    if(first_set_dirty) begin
                        // modify req type to set dirty only, no further resp will be sent
                        pendReq <= Valid (L2TlbRqFromC {
                            child: cRq.child,
                            vpn: cRq.vpn,
                            reqType: SetDirtyOnly,
                            write: True
                        });
                        return True; // start page walk for set dirty only
                    end
                    else begin
                        // req is done
                        pendReq <= Invalid;
                        return False; // no more page walk
                    end
                end
            endactionvalue
            endfunction

            if(respMG.hit) begin
                // hit on a mega or giga page
                let entry = respMG.entry;
                doAssert(entry.page_size > 0 && entry.page_size <= 2, "mega or giga page");
                // check permission
                if(hasSv39Permission(vm_info, cRq.child == I, cRq.write, entry.page_perm)) begin
                    // check dirty bit & update it
                    Bool first_set_dirty = !entry.dirty && cRq.write;
                    entry.dirty = entry.dirty || cRq.write;
                    tlbMG.update(respMG.index, cRq.write);
                    // deq 4KB TLB
                    doAssert(!resp4KB.hit, "4KB page cannot hit");
                    tlb4KB.deqUpdate(False, ?, ?);
                    // check whether we need to resp child & do page walk for set dirty
                    startPageWalk <- pageHit(entry, first_set_dirty, "hit mega/giga");
                end
                else begin
                    pageFault("hit on mega/giga page, no permission");
                end
            end
            else if(resp4KB.hit) begin
                // hit on 4KB page
                let entry = resp4KB.entry;
                doAssert(entry.page_size == 0, "must be 4KB page");
                // check permission
                if(hasSv39Permission(vm_info, cRq.child == I, cRq.write, entry.page_perm)) begin
                    // check dirty bit & update it
                    Bool first_set_dirty = !entry.dirty && cRq.write;
                    entry.dirty = entry.dirty || cRq.write;
                    // deq 4KB TLB & update it
                    tlb4KB.deqUpdate(True, resp4KB.way, SetAssocTlbEntry {valid: True, entry: entry});
                    // check whether we need to resp child & do page walk for set dirty
                    startPageWalk <- pageHit(entry, first_set_dirty, "hit 4KB");
                end
                else begin
                    pageFault("hit on 4KB page, no permission");
                end
            end
            else begin
                // miss, do page walk
                startPageWalk = True;
                // if set dirty only, deq 4KB TLB (no refill)
                if(cRq.reqType == SetDirtyOnly) begin
                    tlb4KB.deqUpdate(False, ?, ?);
                end
            end

            // start page walk
            if(startPageWalk) begin
                // miss, setup page walk
                Bit#(2) level = 2;
                Addr baseAddr = vm_info.base;
                miss <= True;
                walkLevel <= level;
                ptBaseAddr <= baseAddr;
                // req memory (LLC)
                Addr pteAddr = getPTEAddr(baseAddr, cRq.vpn, level);
                memReqQ.enq(TlbMemReq {
                    write: False,
                    addr: pteAddr,
                    data: ?,
                    id: Null
                });
            end
        end
        else begin
            pageFault("not Sv39");
        end
    endrule

    rule doPageWalk(pendReq matches tagged Valid .cRq &&& miss);
        // handle page fault
        function Action pageFault(String reason);
        action
            // page fault, just resp child with invalid entry
            // set dirty req should not incr page fault
            // because it is a hit in DTLB
            if(cRq.reqType == SetDirtyOnly) begin
                doAssert(False, reason + " should not happen for set dirty in doPageWalk");
                // resp has already been sent before
                // 4KB TLB array already deq before
            end
            else begin
                rsToCQ.enq(L2TlbRsToC {
                    child: cRq.child,
                    entry: Invalid
                });
                // 4KB TLB array is not deq yet
                tlb4KB.deqUpdate(False, ?, ?);
            end
            // req is done
            pendReq <= Invalid;
            miss <= False;
        endaction
        endfunction

        // get correct VM info
        VMInfo vm_info = cRq.child == I ? vm_info_I : vm_info_D;

        // get the resp data from memory (LLC)
        respLdQ.deq;
        PTE_Sv39 pte = unpack_PTE_Sv39(respLdQ.first.data);

        if(verbose) begin
            $display("L2TLB page walk: ", fshow(vm_info), " ; ", fshow(cRq), " ; ",
                     fshow(walkLevel), " ; ", fshow(ptBaseAddr), " ; ", fshow(pte));
        end

        if(!pte.valid) begin
            // page fault
            pageFault("invalid page");
        end
        else begin
            // page is valid, check leaf or not
            if(!is_leaf_pte_type(pte.pte_type)) begin
                // non-leaf page
                if(walkLevel == 0) begin
                    // page walk end with non-leaf page -> fault
                    pageFault("non-leaf page at end");
                end
                else begin
                    // continue page walk, update page walk state
                    Addr newPTBase = zeroExtend({pte.ppn2, pte.ppn1, pte.ppn0}) << valueof(PageOffsetSz);
                    Bit#(2) newWalkLevel = walkLevel - 1;
                    walkLevel <= newWalkLevel;
                    ptBaseAddr <= newPTBase;
                    // req memory for PTE
                    Addr newPTEAddr = getPTEAddr(newPTBase, cRq.vpn, newWalkLevel);
                    memReqQ.enq(TlbMemReq {
                        write: False,
                        addr: newPTEAddr,
                        data: ?,
                        id: Null
                    });
                end
            end
            else begin
                // leaf page, check permission
                if(hasSv39Permission(vm_info, cRq.child == I, cRq.write, pte.pte_type)) begin
                    // update PTE ref/dirty bits
                    Bool old_pte_d = pte.d;
                    Bool old_pte_r = pte.r;
                    pte.d = pte.d || cRq.write;
                    pte.r = True;
                    if(old_pte_r != True || old_pte_d != pte.d) begin
                        Addr curPTEAddr = getPTEAddr(ptBaseAddr, cRq.vpn, walkLevel);
                        memReqQ.enq(TlbMemReq {
                            write: True,
                            addr: curPTEAddr,
                            data: pack_PTE_Sv39(pte),
                            id: Null
                        });
                        pendStCnt.incr(1); // incr pending store count
                    end
                    // resp to child
                    if(cRq.reqType == SetDirtyOnly) begin
                        // resp has been sent before, 4KB TLB has been deq before
                    end
                    else begin
                        // get new entry (walkLevel is page size)
                        Vpn masked_vpn = getMaskedVpn(cRq.vpn, walkLevel);
                        Ppn masked_ppn = getMaskedPpn({pte.ppn2, pte.ppn1, pte.ppn0}, walkLevel);
                        let entry = TlbEntry {
                            vpn:       masked_vpn,
                            ppn:       masked_ppn,
                            page_perm: pte.pte_type,
                            page_size: walkLevel,
                            asid:      vm_info.asid,
                            dirty:     pte.d
                        };
                        // resp child
                        rsToCQ.enq(L2TlbRsToC {
                            child: cRq.child,
                            entry: Valid (entry)
                        });
                        // update TLB array
                        if(entry.page_size > 0) begin
                            // add to mega/giga page tlb
                            tlbMG.add_translation(entry);
                            // deq 4KB TLB
                            tlb4KB.deqUpdate(False, ?, ?);
                        end
                        else begin
                            // 4KB page, add to 4KB TLB & deq
                            tlb4KB.deqUpdate(True, tlb4KB.resp.way, SetAssocTlbEntry {valid: True, entry: entry});
                        end
                    end
                    // req is done, miss is resolved
                    pendReq <= Invalid;
                    miss <= False;
                end
                else begin
                    // don't have permission -> fault
                    pageFault("no permission");
                end
            end
        end
    endrule

    method Bool noPendingReqOrWrite;
        return !isValid(pendReq) && pendStCnt == 0;
    endmethod

    method Action updateVMInfo(VMInfo vmI, VMInfo vmD);
        vm_info_I <= vmI;
        vm_info_D <= vmD;
    endmethod

    interface L2TlbToChildren toChildren;
        interface Put rqFromC = toPut(rqFromCQ);
        interface rsToC = toFifoDeq(rsToCQ);
        interface setDirtyRs = toFifoDeq(setDirtyRsQ);

        interface Put iTlbReqFlush;
            method Action put(void x) if(!iFlushReq);
                iFlushReq <= True;
            endmethod
        endinterface
        interface Put dTlbReqFlush;
            method Action put(void x) if(!dFlushReq);
                dFlushReq <= True;
            endmethod
        endinterface
        interface Get flushDone = toGet(flushDoneQ);
    endinterface

    interface TlbMemClient toMem;
        interface FifoDeq memReq = toFifoDeq(memReqQ);
        interface FifoEnq respLd = toFifoEnq(respLdQ);
        interface FifoEnq respSt = toFifoEnq(respStQ);
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
