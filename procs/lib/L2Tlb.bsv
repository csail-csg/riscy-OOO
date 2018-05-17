
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
import Vector::*;
import DefaultValue::*;
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
import LatencyTimer::*;

// for SV39 only

// curretly L2 TLB is just a blocking page walker

// interface with memory (LLC)
typedef enum {Null} TlbMemReqId deriving(Bits, Eq, FShow);

typedef struct {
    // this is always a load req
    Addr addr;
    TlbMemReqId id;
} TlbMemReq deriving(Bits, Eq, FShow);

typedef struct {
    Data data;
    TlbMemReqId id;
} TlbLdResp deriving(Bits, Eq, FShow);

interface TlbMemClient;
    interface FifoDeq#(TlbMemReq) memReq;
    interface FifoEnq#(TlbLdResp) respLd;
endinterface

// interface with children (I/D TLB)
typedef union tagged {
    void I;
    DTlbReqIdx D;
} TlbChild deriving(Bits, Eq, FShow);
typedef struct {
    TlbChild child;
    Vpn vpn;
} L2TlbRqFromC deriving(Bits, Eq, FShow);

typedef struct {
    TlbChild child;
    Maybe#(TlbEntry) entry;
} L2TlbRsToC deriving(Bits, Eq, FShow);

interface L2TlbToChildren;
    interface Put#(L2TlbRqFromC) rqFromC;
    interface FifoDeq#(L2TlbRsToC) rsToC;
    // flush with I/D TLB
    interface Put#(void) iTlbReqFlush;
    interface Put#(void) dTlbReqFlush;
    interface Get#(void) flushDone;
endinterface

interface L2Tlb;
    // keep update with changes to CSRs
    method Action updateVMInfo(VMInfo vmI, VMInfo vmD);

    // ifc with ITLb & DTLB
    interface L2TlbToChildren toChildren;

    // ifc with memory (LLC)
    interface TlbMemClient toMem;

    // performace
    interface Perf#(L2TlbPerfType) perf;
endinterface

typedef FullAssocTlb#(8) L2FullAssocTlb;
module mkL2FullAssocTlb(L2FullAssocTlb);
    let m <- mkFullAssocTlb;
    return m;
endmodule

// TODO we should raise load access fault if the PTE address is not a DRAM
// address. (trap value is still the virtual address being translated).

(* synthesize *)
module mkL2Tlb(L2Tlb::L2Tlb);
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

    // pending req in set assoc TLB pipeline
    Ehr#(2, Maybe#(L2TlbRqFromC)) pendReq_ehr <- mkEhr(Invalid);
    Reg#(Maybe#(L2TlbRqFromC)) pendReq = pendReq_ehr[0];
    Reg#(Maybe#(L2TlbRqFromC)) pendReq_enq = pendReq_ehr[1];

    // page walk currently being prcessed
    Reg#(Bool) miss <- mkReg(False);
    // "i" in riscv spec's page walk algorithm
    Reg#(PageWalkLevel) walkLevel <- mkRegU;
    // page table base addr ("a" in spec's algorithm)
    Reg#(Addr) ptBaseAddr <- mkRegU;
    
    // current processor VM information
    Reg#(VMInfo) vm_info_I <- mkReg(defaultValue);
    Reg#(VMInfo) vm_info_D <- mkReg(defaultValue);

    // Memory Queues for page table walks
    Fifo#(2, TlbMemReq) memReqQ <- mkCFFifo;
    Fifo#(2, TlbLdResp) respLdQ <- mkCFFifo;

    // FIFO for perf req
    Fifo#(1, L2TlbPerfType) perfReqQ <- mkCFFifo;
`ifdef PERF_COUNT
    Fifo#(1, PerfResp#(L2TlbPerfType)) perfRespQ <- mkCFFifo;
    Reg#(Bool) doStats <- mkConfigReg(False);
    Count#(Data) instMissCnt <- mkCount(0);
    Count#(Data) instMissLat <- mkCount(0);
    Count#(Data) dataMissCnt <- mkCount(0);
    Count#(Data) dataMissLat <- mkCount(0);

    LatencyTimer#(2, 12) latTimer <- mkLatencyTimer; // max latency: 4K cycles

    function Action incrMissLat(TlbChild child);
    action
        let lat <- latTimer.done(0);
        if(doStats) begin
            if(child == I) begin
                instMissLat.incr(zeroExtend(lat));
            end
            else begin
                dataMissLat.incr(zeroExtend(lat));
            end
        end
    endaction
    endfunction

    rule doPerf;
        let t <- toGet(perfReqQ).get;
        Data d = (case(t)
            L2TlbInstMissCnt: (instMissCnt);
            L2TlbInstMissLat: (instMissLat);
            L2TlbDataMissCnt: (dataMissCnt);
            L2TlbDataMissLat: (dataMissLat);
            default: (0);
        endcase);
        perfRespQ.enq(PerfResp {
            pType: t,
            data: d
        });
    endrule
`endif

    // when flushing is true, since both I and D TLBs have finished flush and
    // is waiting for L2 to flush, all I/D TLB req must have been responded.
    // Thus, there cannot be any req in pendReq or rqFromCQ. We add the guards
    // to make rules truly exclusive so no req can be processed during flush.
    rule doStartFlush(flushing && !waitFlushDone && !isValid(pendReq));
        waitFlushDone <= True;
        tlb4KB.flush;
        tlbMG.flush;
    endrule

    rule doWaitFlush(flushing && waitFlushDone && tlb4KB.flush_done);
        waitFlushDone <= False;
        flushDoneQ.enq(?);
        iFlushReq <= False;
        dFlushReq <= False;
    endrule

    rule doTlbReq(!isValid(pendReq_enq) && !flushing);
        // get new req
        rqFromCQ.deq;
        let r = rqFromCQ.first;
        // req tlb array
        VMInfo vm_info = r.child == I ? vm_info_I : vm_info_D;
        tlb4KB.req(SetAssocTlbReq {vpn: r.vpn, asid: vm_info.asid});
        // record req
        pendReq_enq <= Valid (r);
        if(verbose) $display("L2TLB new req: ", fshow(r));
    endrule

    // process resp from 4KB TLB and mega-giga TLB
    rule doTlbResp(pendReq matches tagged Valid .cRq &&& !miss);
        doAssert(!flushing, "cannot have pending req when flushing");

        // get correct VM info
        VMInfo vm_info = cRq.child == I ? vm_info_I : vm_info_D;
        doAssert(vm_info.sv39, "must be in sv39 mode");

        // get resp from 4KB TLB and mega-giga TLB
        let resp4KB = tlb4KB.resp;
        let respMG = tlbMG.translate(cRq.vpn, vm_info.asid);

        if(verbose) begin
            $display("L2TLB resp: ", fshow(vm_info), " ; ", fshow(cRq), " ; ", 
                     fshow(resp4KB), " ; ", fshow(respMG));
        end

        // when page hit, resp to child (4KB array is not dequeued)
        function Action pageHit(TlbEntry entry);
        action
            // resp to child
            rsToCQ.enq(L2TlbRsToC {
                child: cRq.child,
                entry: Valid (entry)
            });
            // req is done
            pendReq <= Invalid;
        endaction
        endfunction

        if(!vm_info.sv39) begin
            // not in sv39 -> page fault
            // resp with invalid entry
            rsToCQ.enq(L2TlbRsToC {
                child: cRq.child,
                entry: Invalid
            });
            // 4KB TLB array is not deq yet
            tlb4KB.deqUpdate(None, ?, ?);
            // req is done
            pendReq <= Invalid;
        end
        else if(respMG.hit) begin
            // hit on a mega or giga page
            let entry = respMG.entry;
            doAssert(entry.level > 0 && entry.level <= maxPageWalkLevel,
                     "mega or giga page");
            pageHit(entry);
            tlb4KB.deqUpdate(None, ?, ?); // just deq 4KB array
            tlbMG.updateRep(respMG.index); // update replacement in MG array
        end
        else if(resp4KB.hit) begin
            // hit on 4KB page
            let entry = resp4KB.entry;
            doAssert(entry.level == 0, "must be 4KB page");
            pageHit(entry);
            // update 4KB array replacement, no need to touch MG array
            tlb4KB.deqUpdate(RepInfoOnly, resp4KB.way, ?);
        end
        else begin
            // miss, start page walk
            PageWalkLevel level = maxPageWalkLevel;
            Addr baseAddr = getPTBaseAddr(vm_info.basePPN);
            miss <= True;
            walkLevel <= level;
            ptBaseAddr <= baseAddr;
            // req memory (LLC)
            Addr pteAddr = getPTEAddr(baseAddr, cRq.vpn, level);
            memReqQ.enq(TlbMemReq {
                addr: pteAddr,
                id: Null
            });
            // XXX we keep the 4KB array resp (not deq), because page walk
            // is done in a blocking way
`ifdef PERF_COUNT
            latTimer.start(0);
            if(doStats) begin
                if(cRq.child == I) begin
                    instMissCnt.incr(1);
                end
                else begin
                    dataMissCnt.incr(1);
                end
            end
`endif
        end
    endrule

    rule doPageWalk(pendReq matches tagged Valid .cRq &&& miss);
        doAssert(!flushing, "cannot have pending req when flushing");

        // handle page fault
        function Action pageFault(String reason);
        action
            // resp with invalid entry
            rsToCQ.enq(L2TlbRsToC {
                child: cRq.child,
                entry: Invalid
            });
            // 4KB TLB array is not deq yet
            tlb4KB.deqUpdate(None, ?, ?);
            // req is done
            pendReq <= Invalid;
            miss <= False;
`ifdef PERF_COUNT
            // incr miss latency
            incrMissLat(cRq.child);
`endif
        endaction
        endfunction

        // get correct VM info
        VMInfo vm_info = cRq.child == I ? vm_info_I : vm_info_D;

        // get the resp data from memory (LLC)
        respLdQ.deq;
        PTESv39 pte = unpack(respLdQ.first.data);

        if(verbose) begin
            $display("L2TLB page walk: ", fshow(vm_info), " ; ", fshow(cRq), " ; ",
                     fshow(walkLevel), " ; ", fshow(ptBaseAddr), " ; ", fshow(pte));
        end

        if(!vm_info.sv39) begin
            // no longer in sv39 mode -> page fault
            pageFault("Not in sv39");
        end
        else if(!pte.valid) begin
            // invalid pte -> fault
            pageFault("invalid page");
        end
        else begin
            // page is valid, check leaf or not
            if(!isLeafPTE(pte.pteType)) begin
                // non-leaf page
                if(walkLevel == 0) begin
                    // page walk end with non-leaf page -> fault
                    pageFault("non-leaf page at end");
                end
                else begin
                    // continue page walk, update page walk state
                    Addr newPTBase = getPTBaseAddr(pte.ppn);
                    Bit#(2) newWalkLevel = walkLevel - 1;
                    walkLevel <= newWalkLevel;
                    ptBaseAddr <= newPTBase;
                    // req memory for PTE
                    Addr newPTEAddr = getPTEAddr(newPTBase, cRq.vpn, newWalkLevel);
                    memReqQ.enq(TlbMemReq {
                        addr: newPTEAddr,
                        id: Null
                    });
                end
            end
            else begin
                // leaf page, get new entry
                Vpn masked_vpn = getMaskedVpn(cRq.vpn, walkLevel);
                Ppn masked_ppn = getMaskedPpn(pte.ppn, walkLevel);
                let entry = TlbEntry {
                    vpn:     masked_vpn,
                    ppn:     masked_ppn,
                    pteType: pte.pteType,
                    level:   walkLevel,
                    asid:    vm_info.asid
                };
                // resp child
                rsToCQ.enq(L2TlbRsToC {
                    child: cRq.child,
                    entry: Valid (entry)
                });
                // update TLB array
                if(entry.level > 0) begin
                    // add to mega/giga page tlb
                    tlbMG.addEntry(entry);
                    // deq 4KB TLB
                    tlb4KB.deqUpdate(None, ?, ?);
                end
                else begin
                    // 4KB page, add to 4KB TLB & deq
                    tlb4KB.deqUpdate(NewEntry, tlb4KB.resp.way, entry);
                end
                // req is done, miss is resolved
                pendReq <= Invalid;
                miss <= False;
`ifdef PERF_COUNT
                // incr miss latency
                incrMissLat(cRq.child);
`endif
            end
        end
    endrule

    method Action updateVMInfo(VMInfo vmI, VMInfo vmD); //if(!isValid(pendReq));
        vm_info_I <= vmI;
        vm_info_D <= vmD;
    endmethod

    interface L2TlbToChildren toChildren;
        interface Put rqFromC = toPut(rqFromCQ);
        interface rsToC = toFifoDeq(rsToCQ);

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
    endinterface
  
    interface Perf perf;
        method Action setStatus(Bool stats);
`ifdef PERF_COUNT
            doStats <= stats;
`else
            noAction;
`endif
        endmethod

        method Action req(L2TlbPerfType r);
            perfReqQ.enq(r);
        endmethod

        method ActionValue#(PerfResp#(L2TlbPerfType)) resp;
`ifdef PERF_COUNT
            perfRespQ.deq;
            return perfRespQ.first;
`else
            perfReqQ.deq;
            return PerfResp {
                pType: perfReqQ.first,
                data: 0
            };
`endif
        endmethod

        method Bool respValid;
`ifdef PERF_COUNT
            return perfRespQ.notEmpty;
`else
            return perfReqQ.notEmpty;
`endif
        endmethod
    endinterface
endmodule
