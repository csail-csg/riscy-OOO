
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
import Types::*;
import ProcTypes::*;
import Vector::*;
import GetPut::*;
import Assert::*;
import Ehr::*;
import HasSpecBits::*;
import SpecFifo::*;
import StoreBuffer::*;
import Exec::*;

// I don't want to export auxiliary functions, so manually export all types
export LSQState(..);
export LSQDeqEntry(..);
export LSQUpdateResult(..);
export LSQForwardResult(..);
export LSQIssueResult(..);
export LSQIssueInfo(..);
export LSQKillInfo(..);
export LSQRespLdResult(..);
export LSQHitInfo(..);
export SpecLSQ(..);
export mkSpecLSQ;

// state transition
// Ld: enq and Idle -> set computed |-> issue and Executing |-> resp and Done |-> Deq
//                                  |                       |                 |-> get killed and set ldKilled
//                                  |                       |-> get killed and set ldKilled
//                                  |-> issue fail and set ldDepXXX -> reset ldDepXXX
// St: enq and Idle -> set computed -> deq
// Lr/Sc/Amo: enq and Idle -> set computed -> issue to mem -> get resp and deq
// Fence: enq and Idle -> deq

// XXX Currently I rely on the fact that specbits of an entry in LSQ contain the spectag of itself
// so I could kill load easily via specUpdate interface when load speculation fails
// XXX The load specbits in ROB must contain the spectag of itself right after address translation
// because any exception caused by addr translation should kill LSQ entry but not ROB entry
// but failed load speculation should kill both ROB and LSQ entries

// XXX When detecting eagerly executed loads, we only consider aligned addr for dword granularity
// This reduces the complexity of checking, i.e. we don't consider the byte en of each mem access
// XXX This forces all accesses to the same dword addr to be executed in order
// XXX I choose to kill an eager load L even when it is still executing.
// An alternative way is to re-execute L. However, this won't work well with bypassing load result.
// Consider L is forced to re-execute because an older load L1 updates the LSQ.
// When L got into the cache, it wakes up another load L2 which depends on the result of L.
// L will wait for L1 to first execute. Suppose the store set says that L1 may dependend on a store S.
// Then L2 will stuck at the reg read stage before S resolves its addr.
// However, if S is waken up from reservation station after L2, then we deadlock.

// TODO Fence is currently not inserted into LSQ
// we need to set its aq/rl bits in future and put it into LSQ

// XXX I implemented two flavors of associative searches
// First is using virtual tags, this involves computing the virtual tags
// Second is using normal search, this requires more numbers of searches
// The choice is controlled by macro LSQ_VTAG

typedef enum {
    Idle, // Ld is waiting for execution
    Executing, // Ld is being forwarded or in cache
    Done // memory instruction is done
} LSQState deriving(Bits, Eq, FShow);

typedef struct {
    InstTag          inst_tag;
    Addr             pc; // for Ld speculation failure
    MemInst          mem_inst; // from decode
    Maybe#(PhyDst)   dst;
    Addr             paddr;
    Bool             isMMIO; // whether paddr is mmio addr; MMIO access is handled at deq time non-speculatively
    ByteEn           shiftedBE; // store byte enable after shift to align with dword boudary
    // XXX even for Amo this BE **IS** shifted to detect address overlap
    Data             shiftedData; // store data or load result
    // for Ld: XXX load result after shift, i.e. the data written to reg file (cannot used for bypass to younger loads)
    // for St/Sc: store data after shift data to align with dword boudary
    // for Amo: XXX store data **NOT** shifted, this doesn't affect forwarding to Ld, because AMO never forwards data
    LSQState         state; // only Ld can be non-Idle state
    Bool             computed; // paddr/isMMIO/data have been computed
    // reasons for Ld stall, can be non-Invalid only when state == Idle
    Maybe#(LdStQTag) ldDepLSQDeq; // Ld stalled by Lr/Sc/Amo/St for same addr or Reconcile, should wait it to deq
    Maybe#(LdStQTag) ldDepLdEx; // Ld stalled by unexecuted Ld to same addr, should wait it to be issued
    Maybe#(SBIndex)  ldDeqSB; // Ld stalled by store buffer entry, should wait it to write cache
    Bool             ldInIssueQ; // Ld is in issueQ, can be true only when state == Idle
    Bool             ldKilled; // Ld is killed by older inst (failed speculation), can be true only when state == Done/Executing
    // I don't set computed/depSrc/InIssueQ/ReEx/Killed as separate LSQState to avoid conflicts on the state field
    Maybe#(SpecTag)  specTag; // spec tag for Ld or MMIO
    SpecBits         spec_bits; // spec bits contain the spec tag of the mem inst itself
    Bool             waitWPResp; // pending on a wrong path load resp (if current entry is Ld/Lr/Sc/Amo, it cannot issue)
} LSQEntry deriving (Bits, Eq, FShow);

typedef struct {
    Bool waitWPResp;
} LSQUpdateResult deriving(Bits, Eq, FShow);

typedef struct {
    Maybe#(PhyDst) dst;
    Data data; // align with dword, not final result written to reg file
} LSQForwardResult deriving(Bits, Eq, FShow);

typedef union tagged {
    void ToCache;
    void Stall;
    LSQForwardResult Forward;
} LSQIssueResult deriving(Bits, Eq, FShow);

typedef struct {
    LdStQTag tag;
    Addr paddr;
    ByteEn shiftedBE;
} LSQIssueInfo deriving(Bits, Eq, FShow);

typedef struct {
    Addr pc;
    InstTag inst_tag;
    Maybe#(SpecTag) specTag;
} LSQKillInfo deriving(Bits, Eq, FShow);

typedef struct {
    Bool wrongPath;
    Maybe#(PhyDst) dst;
    Data data;
} LSQRespLdResult deriving(Bits, Eq, FShow);

typedef struct {
    Bool waitWPResp;
    Maybe#(PhyDst) dst;
} LSQHitInfo deriving(Bits, Eq, FShow);

typedef struct {
    LdStQTag        ldstq_tag;
    InstTag         inst_tag;
    MemInst         mem_inst;
    Maybe#(PhyDst)  dst;
    Addr            paddr;
    Bool            isMMIO;
    ByteEn          shiftedBE;
    Data            shiftedData;
    LSQState        state;
    Bool            computed;
    Bool            ldKilled;
    Maybe#(SpecTag) specTag;
    SpecBits        spec_bits;
    Bool            waitWPResp;
} LSQDeqEntry deriving (Bits, Eq, FShow);

interface SpecLSQ;
    // enq at renaming
    method Bool canEnq;
    method Action enq(InstTag newInstTag, Addr newPc, MemInst newMemInst, Maybe#(PhyDst) newDst, SpecBits newSpecBits);
    method LdStQTag enqTag;
    // MEM inst needs PC at addr translation
    method Addr getOrigPC(LdStQTag t);
    // MEM inst needs orignal BE (not aligned) at addr translation
    method ByteEn getOrigBE(LdStQTag t);
    // retrieve information when we want to wakeup RS early in case Ld/Lr/Sc/Amo hits in cache
    method ActionValue#(LSQHitInfo) getHit(LdStQTag t);
    // update addr/data after address translation and data computation (if applicable)
    // and set the spec tag if it is a load
    // also search for the (oldest) younger load to kill
    method ActionValue#(LSQUpdateResult) update(
        LdStQTag lsqTag, Addr paddr, Bool isMMIO, ByteEn shiftedBE, Data shiftedData, Maybe#(SpecTag) specTag
    );
    // issue a load, and remove dependence on this load issue
    method ActionValue#(LSQIssueResult) issue(LdStQTag lsqTag, Addr paddr, ByteEn shiftedBE, SBSearchRes sbRes);
    // get the load to issue
    method ActionValue#(LSQIssueInfo) getIssueLd;
    // get the load to kill
    method ActionValue#(LSQKillInfo) getKillLd;
    // get load resp
    // we wake up stalled loads in respLd if the resp is not dropped
    // we hold load result in LSQ because we set ROB entry Executed at LSQ deq time
    // and we need to set ROB entry full_result at that deq time
    method ActionValue#(LSQRespLdResult) respLd(LdStQTag t, Data alignedData);
    // deq entry, and wakeup stalled loads
    // XXX outside world should do the following
    // (1) sends Lr/Sc/Amo to cache only at deq port
    // (2) check specBits & waitWPResp before issue Lr/Sc/Amo
    // (3) check specBits & state before deq any mem inst
    // (4) check ldKilled before deq Ld (if ldKilled, then Ld cannot deq)
    // (5) set ROB entry of deq mem inst to Executed (so that ROB can commit)
    // (6) when deq Ld, clear spectag of Ld globally
    method LSQDeqEntry first;
    method Action deq;
    // wake up younger loads when SB deq
    method Action wakeupLdStalledBySB(SBIndex sbIdx);
    // speculation
    interface SpeculationUpdate specUpdate;
endinterface

// --- auxiliary types and functions ---
`ifdef LSQ_VTAG
// virtual index: 0 -- 2*LSQSize-1
typedef Bit#(TLog#(TMul#(2, LdStQSize))) LSQVTag;
`endif

typedef Bit#(TSub#(AddrSz, TLog#(NumBytes))) DataAlignedAddr;
function DataAlignedAddr getDataAlignedAddr(Addr a) = truncateLSB(a);

// whether two memory access are to the same dword
function Bool sameAlignedAddr(Addr a, Addr b);
    return getDataAlignedAddr(a) == getDataAlignedAddr(b);
endfunction

// check whether mem op addr is aligned w.r.t data size
function Bool checkAddrAlign(Addr paddr, ByteEn be);
    Bit#(TLog#(NumBytes)) byteOffset = truncate(paddr);
    if(be[7]) begin
        return byteOffset == 0;
    end
    else if(be[3]) begin
        return byteOffset[1:0] == 0;
    end
    else if(be[1]) begin
        return byteOffset[0] == 0;
    end
    else begin
        return True;
    end
endfunction

// issueQ of LSQ tags for issue
typedef SpecFifo_SB_deq_enq_SB_deq_wrong_C_enq#(2, LSQIssueInfo) LSQIssueQ;
(* synthesize *)
module mkLSQIssueQ(LSQIssueQ);
    let m <- mkSpecFifo_SB_deq_enq_SB_deq_wrong_C_enq(True);
    return m;
endmodule

// killQ of LSQ tags should be killed
typedef SpecFifo_SB_deq_enq_SB_deq_wrong_C_enq#(2, LdStQTag) LSQKillQ;
(* synthesize *)
module mkLSQKillQ(LSQKillQ);
    let m <- mkSpecFifo_SB_deq_enq_SB_deq_wrong_C_enq(True);
    return m;
endmodule
// --- end of auxiliary types and functions ---

(* synthesize *)
module mkSpecLSQ(SpecLSQ);
    // ordering
    // getKillLd < findIssue < deq < update < issue, getIssueLd <
    // enqIssueQ < respLd, wakeupLdStalledBySB (wakeSB) < enq < correctSpec

    Integer valid_wrongSpec_port = 0;
    Integer valid_getKill_port = 0;
    Integer valid_findIss_port = 0;
    Integer valid_deq_port = 0; // write valid
    Integer valid_update_port = 1;
    Integer valid_issue_port = 1;
    Integer valid_enqIss_port = 1;
    Integer valid_respLd_port = 1;
    Integer valid_enq_port = 1; // write valid

    Integer paddr_findIss_port = 0;
    Integer paddr_deq_port = 0;
    Integer paddr_update_port = 0; // write paddr
    Integer paddr_issue_port = 1;
    Integer paddr_enqIss_port = 1;
    Integer paddr_respLd_port = 1;

    Integer mmio_findIss_port = 0;
    Integer mmio_deq_port = 0;
    Integer mmio_update_port = 0; // write isMMIO
    Integer mmio_issue_port = 1;
    Integer mmio_enqIss_port = 1;

    Integer shBE_findIss_port = 0;
    Integer shBE_deq_port = 0;
    Integer shBE_update_port = 0; // write shiftedBE
    Integer shBE_issue_port = 1;
    Integer shBE_enqIss_port = 1;

    Integer shData_deq_port = 0;
    Integer shData_update_port = 0; // write shiftedData
    Integer shData_issue_port = 1;
    Integer shData_respLd_port = 1; // write shiftedData

    Integer state_wrongSpec_port = 0;
    Integer state_getKill_port = 0;
    Integer state_findIss_port = 0;
    Integer state_deq_port = 0;
    Integer state_update_port = 0;
    Integer state_issue_port = 0; // write state
    Integer state_enqIss_port = 1;
    Integer state_respLd_port = 1; // write state
    Integer state_enq_port = 2; // write state

    Integer comp_findIss_port = 0;
    Integer comp_deq_port = 0;
    Integer comp_update_port = 0; // write computed
    Integer comp_issue_port = 1;
    Integer comp_enqIss_port = 1;
    Integer comp_enq_port = 1; // write computed

    Integer depLSQDeq_findIss_port = 0;
    Integer depLSQDeq_deq_port = 0; // write ldDepLSQDeq
    Integer depLSQDeq_issue_port = 1; // write ldDepLSQDeq
    Integer depLSQDeq_enqIss_port = 2;
    Integer depLSQDeq_enq_port = 2; // write ldDepLSQDeq

    Integer depLdEx_findIss_port = 0;
    Integer depLdEx_issue_port = 0; // write ldDepLdEx
    Integer depLdEx_enqIss_port = 1;
    Integer depLdEx_enq_port = 1; // write ldDepLdEx

    Integer depSB_findIss_port = 0;
    Integer depSB_issue_port = 0; // write ldDepSB
    Integer depSB_enqIss_port = 1;
    Integer depSB_wakeSB_port = 1; // write ldDepSB
    Integer depSB_enq_port = 2; // write ldDepSB

    Integer inIssQ_findIss_port = 0;
    Integer inIssQ_issue_port = 0; // write ldInIssueQ
    Integer inIssQ_enqIss_port = 1; // write ldInIssueQ
    Integer inIssQ_enq_port = 2; // write ldInIssueQ

    Integer killed_getKill_port = 0;
    Integer killed_deq_port = 0;
    Integer killed_update_port = 0; // write ldKilled
    Integer killed_issue_port = 1;
    Integer killed_enqIss_port = 1;
    Integer killed_enq_port = 1; // write ldKilled

    Integer specTag_getKill_port = 0;
    Integer specTag_deq_port = 0;
    Integer specTag_update_port = 0; // write specTag
    Integer specTag_enq_port = 1; // write specTag

    Integer sb_wrongSpec_port = 0;
    Integer sb_deq_port = 0;
    Integer sb_update_port = 0;
    Integer sb_enqIss_port = 0;
    Integer sb_enq_port = 0; // write specBits
    Integer sb_correctSpec_port = 1; // write specBits

    Integer deqP_wrongSpec_port = 0;
    Integer deqP_findIss_port = 0;
    Integer deqP_deq_port = 0; // write deqP
    Integer deqP_issue_port = 1;

    // entry valid bits
    Vector#(LdStQSize, Ehr#(2, Bool))             valid       <- replicateM(mkEhr(False));
    // entry contents
    Vector#(LdStQSize, Reg#(InstTag))             instTag     <- replicateM(mkRegU);
    Vector#(LdStQSize, Reg#(Addr))                pc          <- replicateM(mkRegU);
    Vector#(LdStQSize, Reg#(MemInst))             memInst     <- replicateM(mkRegU);
    Vector#(LdStQSize, Reg#(Maybe#(PhyDst)))      dst         <- replicateM(mkRegU);
    Vector#(LdStQSize, Ehr#(2, Addr))             paddr       <- replicateM(mkEhr(?));
    Vector#(LdStQSize, Ehr#(2, Bool))             isMMIO      <- replicateM(mkEhr(?));
    Vector#(LdStQSize, Ehr#(2, ByteEn))           shiftedBE   <- replicateM(mkEhr(?));
    Vector#(LdStQSize, Ehr#(2, Data))             shiftedData <- replicateM(mkEhr(?));
    Vector#(LdStQSize, Ehr#(3, LSQState))         state       <- replicateM(mkEhr(?));
    Vector#(LdStQSize, Ehr#(2, Bool))             computed    <- replicateM(mkEhr(?));
    Vector#(LdStQSize, Ehr#(3, Maybe#(LdStQTag))) ldDepLSQDeq <- replicateM(mkEhr(?));
    Vector#(LdStQSize, Ehr#(2, Maybe#(LdStQTag))) ldDepLdEx   <- replicateM(mkEhr(?));
    Vector#(LdStQSize, Ehr#(3, Maybe#(SBIndex)))  ldDepSB     <- replicateM(mkEhr(?));
    Vector#(LdStQSize, Ehr#(3, Bool))             ldInIssueQ  <- replicateM(mkEhr(?));
    Vector#(LdStQSize, Ehr#(2, Bool))             ldKilled    <- replicateM(mkEhr(?));
    Vector#(LdStQSize, Ehr#(2, Maybe#(SpecTag)))  specTag     <- replicateM(mkEhr(?));
    Vector#(LdStQSize, Ehr#(2, SpecBits))         specBits    <- replicateM(mkEhr(?));
    // wrong-path load filter (must init to all False)
    Vector#(LdStQSize, Reg#(Bool))                waitWPResp  <- replicateM(mkReg(False));
    // LSQ enq/deq ptr
    Reg#(LdStQTag) enqP <- mkReg(0);
    Ehr#(2, LdStQTag) deqP <- mkEhr(0);

    // FIFO of LSQ tags that try to issue, there should be no replication in it
    LSQIssueQ issueQ <- mkLSQIssueQ;
    // XXX we split the search for ready to issue entry into two phases
    // Phase 1: rule findIssue: find a ready-to-issue entry at the beginning of the cycle
    // Phase 2: rule enqIssueQ: enq the one found in findIssue into issueQ and set ldInIssueQ
    // We do the split because enq to issueQ must be ordered after getIssueLd method which deq issueQ
    // This split is fine because at phase 2, the entry found in phase one should not be changed by any other method
    // This is because findIssue < update < issue < enqIssueQ
    // i.e. update and issue will not affect the entry found in findIssue
    // We use a wire to pass phase 1 result to phase 2
    // It is fine that phase dose not fire when phase 1 has fired, next cycle phase 1 will redo the work
    RWire#(LSQIssueInfo) issueLd <- mkRWire;

    // FIFO of LSQ tags that should be killed
    // note that replicated tags may exist
    // but replications will all be killed when the first is processed due to spec bits
    LSQKillQ killQ <- mkLSQKillQ;

    // make incorrectSpeculation conflict with all others (but not correctSpec method and findIssue)
    RWire#(void) wrongSpec_hit_conflict <- mkRWire;
    RWire#(void) wrongSpec_enqIss_conflict <- mkRWire;
    RWire#(void) wrongSpec_enq_conflict <- mkRWire;
    RWire#(void) wrongSpec_update_conflict <- mkRWire;
    RWire#(void) wrongSpec_issue_conflict <- mkRWire;
    RWire#(void) wrongSpec_respLd_conflict <- mkRWire;
    RWire#(void) wrongSpec_deq_conflict <- mkRWire;
    RWire#(void) wrongSpec_wakeBySB_conflict <- mkRWire;

    function LdStQTag getNextPtr(LdStQTag t);
        return t == fromInteger(valueOf(LdStQSize) - 1) ? 0 : t + 1;
    endfunction

    function Bool hasReconcile(LdStQTag t);
        return memInst[t].aq; // XXX currently no fence in LSQ, only check acquire bit
    endfunction

`ifdef LSQ_VTAG
    // Since enqP is not changed during all our associative searches,
    // we map LSQ index to virtual tags using enqP as pivot.
    // The mapping is as follow:
    // - valid entry i --> i < enqP ? i + LdStQSize : i
    // - deqP, same as valid entry
    // - enqP --> enqP + LdStQSize
    // NOTE that this mapping is pathological when LSQ is empty,
    // so we must check empty before using virtual tags.
    // Fortunately, all associative searches look for valid entries,
    // so we must always be correct when LSQ is empty.
    function LSQVTag getEntryVirtualTag(LdStQTag i);
        return i < enqP ? zeroExtend(i) + fromInteger(valueof(LdStQSize)) : zeroExtend(i);
    endfunction
    // virtual tags for all entries (which can be reused in all associative searches)
    Vector#(LdStQSize, LSQVTag) virtualTags = map(getEntryVirtualTag, genWith(fromInteger));

    // find oldest entry that satisfy certain constraint (i.e. smallest tag)
    function Maybe#(LdStQTag) findOldest(Vector#(LdStQSize, Bool) pred);
        function LdStQTag getOlder(LdStQTag a, LdStQTag b);
            if(!pred[a]) begin
                return b;
            end
            else if(!pred[b]) begin
                return a;
            end
            else begin
                return virtualTags[a] < virtualTags[b] ? a : b;
            end
        endfunction
        Vector#(LdStQSize, LdStQTag) idxVec = genWith(fromInteger);
        LdStQTag tag = fold(getOlder, idxVec);
        return pred[tag] ? Valid (tag) : Invalid;
    endfunction

    // find youngest entry that satisfy certain constraint (i.e. largest tag)
    function Maybe#(LdStQTag) findYoungest(Vector#(LdStQSize, Bool) pred);
        function LdStQTag getOlder(LdStQTag a, LdStQTag b);
            if(!pred[a]) begin
                return b;
            end
            else if(!pred[b]) begin
                return a;
            end
            else begin
                return virtualTags[a] < virtualTags[b] ? b : a;
            end
        endfunction
        Vector#(LdStQSize, LdStQTag) idxVec = genWith(fromInteger);
        LdStQTag tag = fold(getOlder, idxVec);
        return pred[tag] ? Valid (tag) : Invalid;
    endfunction
`endif

    // find load ready for issuing when LSQ is not empty:
    // (1) entry valid of load
    // (2) state is Idle
    // (3) computed is True
    // (4) not depend on any thing
    // (5) waitWPResp is False
    // (6) not in issueQ
    // (7) not MMIO
    // Since this rule does not block any other rule, we can let it fire even when it may do nothing
    rule findIssue;
        function Bool canIssue(LdStQTag i);
            return (
                valid[i][valid_findIss_port] && memInst[i].mem_func == Ld && // (1) valid load
                state[i][state_findIss_port] == Idle && // (2) Idle state
                computed[i][comp_findIss_port] && // (3) computed addr
                !isValid(ldDepLSQDeq[i][depLSQDeq_findIss_port]) &&
                !isValid(ldDepLdEx[i][depLdEx_findIss_port]) &&
                !isValid(ldDepSB[i][depSB_findIss_port]) && // (4) no dependency
                !waitWPResp[i] && // (5) not wating wrong path resp
                !ldInIssueQ[i][inIssQ_findIss_port] && // (6) not in issueQ
                !isMMIO[i][mmio_findIss_port] // (7) not MMIO
            );
        endfunction
        Vector#(LdStQSize, LdStQTag) idxVec = genWith(fromInteger); // index vector
        Vector#(LdStQSize, Bool) isAbleToIssue = map(canIssue, idxVec);

        // find the oldest load to issue (note that we search for valid entry)
        Maybe#(LdStQTag) issueTag;
`ifdef LSQ_VTAG
        // use virtual tag
        issueTag = findOldest(isAbleToIssue);
`else
        // use normal search: do two rounds
        // first search smallest tag among entries >= deqP
        LdStQTag curDeqP = deqP[deqP_findIss_port];
        function Bool canIssueFirst(LdStQTag i);
            return curDeqP <= i && isAbleToIssue[i];
        endfunction
        Maybe#(LdStQTag) firstTag = find(canIssueFirst, idxVec);
        // next search smallest tag among all entries
        function Bool canIssueNext(LdStQTag i) = isAbleToIssue[i];
        Maybe#(LdStQTag) nextTag = find(canIssueNext, idxVec);
        // merge
        issueTag = isValid(firstTag) ? firstTag : nextTag;
`endif

        // set wire
        if(issueTag matches tagged Valid .tag) begin
            issueLd.wset(LSQIssueInfo {
                tag: tag,
                paddr: paddr[tag][paddr_findIss_port],
                shiftedBE: shiftedBE[tag][shBE_findIss_port]
            });
        end
    endrule

    rule enqIssueQ(issueLd.wget matches tagged Valid .info);
        // sanity check
        doAssert(valid[info.tag][valid_enqIss_port], "enq issueQ entry is valid");
        doAssert(memInst[info.tag].mem_func == Ld, "enq issueQ entry is Ld");
        doAssert(computed[info.tag][comp_enqIss_port], "enq issueQ entry is computed");
        doAssert(state[info.tag][state_enqIss_port] == Idle, "enq issueQ entry should be idle");
        doAssert(!ldInIssueQ[info.tag][inIssQ_enqIss_port], "enq issueQ entry cannot be in issueQ");
        doAssert(!ldKilled[info.tag][killed_enqIss_port], "enq issueQ entry cannot be ldKilled");
        doAssert(!waitWPResp[info.tag], "enq issueQ entry cannot wait for wrong path resp");
        doAssert(!isMMIO[info.tag][mmio_enqIss_port], "enq issueQ entry cannot be MMIO");
        doAssert(
            !isValid(ldDepLSQDeq[info.tag][depLSQDeq_enqIss_port]) &&
            !isValid(ldDepLdEx[info.tag][depLdEx_enqIss_port]) &&
            !isValid(ldDepSB[info.tag][depSB_enqIss_port]),
            "enq issueQ entry cannot have dependency"
        );
        doAssert(info.shiftedBE == shiftedBE[info.tag][shBE_enqIss_port], "BE should match");
        doAssert(info.paddr == paddr[info.tag][paddr_enqIss_port], "paddr should match");
        // enq to issueQ & change state (prevent enq this tag again)
        issueQ.enq(ToSpecFifo {
            data: info,
            spec_bits: specBits[info.tag][sb_enqIss_port]
        });
        ldInIssueQ[info.tag][inIssQ_enqIss_port] <= True;
        // make conflict with incorrect spec
        wrongSpec_enqIss_conflict.wset(?);
    endrule

`ifdef BSIM
    // sanity check in simulation
    // all valid entry are consective within deqP and enqP, outsiders are invalid entries
    (* fire_when_enabled, no_implicit_conditions *)
    rule sanityCheck;
        Bool allEmpty = all( \== (False),  readVEhr(0, valid) );
        function Bool in_range(LdStQTag i);
            // if i is within deqP and enqP, i.e. should be valid entry
            if(allEmpty) begin
                return False;
            end
            else begin
                if(deqP[0] < enqP) begin
                    return deqP[0] <= i && i < enqP;
                end
                else begin
                    return deqP[0] <= i || i < enqP;
                end
            end
        endfunction
        for(Integer i = 0; i < valueof(LdStQSize); i = i+1) begin
            doAssert(in_range(fromInteger(i)) == valid[i][0],
                "entries inside deqP and enqP should be valid, otherwise invalid"
            );
        end
    endrule
`endif

    // lazy enq
    Wire#(Bool) can_enq_wire <- mkBypassWire;
    (* fire_when_enabled, no_implicit_conditions *)
    rule setForEnq;
        can_enq_wire <= !valid[enqP][0];
    endrule

    method Addr getOrigPC(LdStQTag t);
        return pc[t];
    endmethod

    method ByteEn getOrigBE(LdStQTag t);
        return memInst[t].byteEn;
    endmethod

    method ActionValue#(LSQHitInfo) getHit(LdStQTag t);
        wrongSpec_hit_conflict.wset(?);
        return LSQHitInfo {
            waitWPResp: waitWPResp[t],
            dst: dst[t]
        };
    endmethod

    method Bool canEnq = can_enq_wire;

    method Action enq(
        InstTag newInstTag, Addr newPc, MemInst newMemInst,
        Maybe#(PhyDst) newDst, SpecBits newSpecBits
    ) if(can_enq_wire);
        doAssert(!valid[enqP][valid_enq_port], "entry at enqP must be invalid");
        valid[enqP][valid_enq_port] <= True;
        enqP <= getNextPtr(enqP);
        // set up entry
        instTag[enqP] <= newInstTag;
        pc[enqP] <= newPc;
        memInst[enqP] <= newMemInst;
        dst[enqP] <= newDst;
        state[enqP][state_enq_port] <= Idle;
        computed[enqP][comp_enq_port] <= False;
        ldDepLSQDeq[enqP][depLSQDeq_enq_port] <= Invalid;
        ldDepLdEx[enqP][depLdEx_enq_port] <= Invalid;
        ldDepSB[enqP][depSB_enq_port] <= Invalid;
        ldInIssueQ[enqP][inIssQ_enq_port] <= False;
        ldKilled[enqP][killed_enq_port] <= False;
        specTag[enqP][specTag_enq_port] <= Invalid;
        specBits[enqP][sb_enq_port] <= newSpecBits;
        // make conflict with incorrect spec
        wrongSpec_enq_conflict.wset(?);
    endmethod

    method enqTag = enqP;

    method ActionValue#(LSQUpdateResult) update(
        LdStQTag lsqTag, Addr pa, Bool mmio, ByteEn shift_be, Data shift_data, Maybe#(SpecTag) spec_tag
    );
        // sanity check
        doAssert(valid[lsqTag][valid_update_port], "updating entry must be valid");
        doAssert(state[lsqTag][state_update_port] == Idle && !computed[lsqTag][comp_update_port],
            "updating entry should be in Idle state and not computed yet"
        );
        doAssert(!computed[lsqTag][comp_update_port], "updating entry cannot be computed");
        doAssert(isValid(spec_tag) == (mmio || memInst[lsqTag].mem_func == Ld),
            "only Ld or MMIO needs to set spec tag"
        );
        case(memInst[lsqTag].mem_func)
            Ld, St, Lr, Sc, Amo: noAction;
            default: doAssert(False, "updating entry cannot be fence");
        endcase

        // write computed, paddr, shift be, shift data, specTag
        computed[lsqTag][comp_update_port] <= True;
        paddr[lsqTag][paddr_update_port] <= pa;
        isMMIO[lsqTag][mmio_update_port] <= mmio;
        shiftedBE[lsqTag][shBE_update_port] <= shift_be;
        shiftedData[lsqTag][shData_update_port] <= shift_data; // data has been shifted for non-AMO
        specTag[lsqTag][specTag_update_port] <= spec_tag;

        // index vec for vector functions
        Vector#(LdStQSize, LdStQTag) idxVec = genWith(fromInteger);

        // kill younger & eager load
        // We search from lsqTag to the youngest entry
        // Find the first valid and computed entry for the same aligned addr (XXX fence cannot be computed)
        // We kill this entry if it is an executing or done Ld.
        // No need to check further entries, because if the found entry is not killed,
        // then further entries must have been killed or stalled by the found entry
        function Bool checkEntry(LdStQTag i);
            return valid[i][valid_update_port] && computed[i][comp_update_port] &&
                   sameAlignedAddr(pa, paddr[i][paddr_update_port]);
        endfunction
        Vector#(LdStQSize, Bool) needCheck = map(checkEntry, idxVec);
        // find the entry to check
        Maybe#(LdStQTag) checkTag = Invalid;
`ifdef LSQ_VTAG
        // use virtual tag: only consider entries younger than lsqTag (i.e. larger virtual tag)
        // note that LSQ must be non-empty, and we search for valid entry
        LSQVTag virtualLsqTag = virtualTags[lsqTag];
        function Bool checkYoungerEntry(LdStQTag i);
            return needCheck[i] && virtualTags[i] > virtualLsqTag;
        endfunction
        // get the oldest one
        checkTag = findOldest(map(checkYoungerEntry, idxVec));
`else
        // normal search: two cases (LSQ must be non-empty)
        if(lsqTag < enqP) begin
            // younger entries are in (lsqTag, enqP), search from small tag to large
            function Bool checkYoungerEntry(LdStQTag i);
                return i > lsqTag && i < enqP && needCheck[i];
            endfunction
            checkTag = find(checkYoungerEntry, idxVec);
        end
        else begin
            // younger entries are in (lsqTag, LdStQSize) + [0, enqP)
            // first search from small tag to large within (lsqTag, LdStQSize)
            function Bool checkEntryFirst(LdStQTag i);
                return i > lsqTag && needCheck[i];
            endfunction
            Maybe#(LdStQTag) firstTag = find(checkEntryFirst, idxVec);
            // next search from small tag t olarge within [0, enqP)
            function Bool checkEntryNext(LdStQTag i);
                return i < enqP && needCheck[i];
            endfunction
            Maybe#(LdStQTag) nextTag = find(checkEntryNext, idxVec);
            // merge
            checkTag = isValid(firstTag) ? firstTag : nextTag;
        end
`endif
        // check for killing Ld (executing/done): mark it as killed & enq killQ
        if(checkTag matches tagged Valid .tag &&& memInst[tag].mem_func == Ld &&& state[tag][state_update_port] != Idle) begin
            ldKilled[tag][killed_update_port] <= True;
            killQ.enq(ToSpecFifo {
                data: tag,
                spec_bits: specBits[tag][sb_update_port]
            });
            doAssert(!isMMIO[tag][mmio_update_port], "cannot kill MMIO Ld");
            // when the Ld called incorrectSpeculation, it will kill itself
            // and set waitWPResp if it is still executing at that time
            Maybe#(SpecTag) st = specTag[tag][specTag_update_port];
            doAssert(isValid(st), "killed Ld must have spec tag");
            doAssert(specBits[tag][sb_update_port][validValue(st)] == 1, "sb of killed Ld has itself's spec tag");
        end

        // make conflict with incorrect spec
        wrongSpec_update_conflict.wset(?);

        // return waiting for wp resp bit: for deciding whether the updating Ld can be issued
        return LSQUpdateResult {
            waitWPResp: waitWPResp[lsqTag]
        };
    endmethod

    method ActionValue#(LSQIssueResult) issue(LdStQTag lsqTag, Addr pa, ByteEn shift_be, SBSearchRes sbRes);
        doAssert(pa == paddr[lsqTag][paddr_issue_port], "Ld paddr incorrect");
        doAssert(shift_be == shiftedBE[lsqTag][shBE_issue_port], "Ld BE incorrect");
        doAssert(valid[lsqTag][valid_issue_port], "issuing Ld must be valid");
        doAssert(computed[lsqTag][comp_issue_port], "issuing Ld must be computed");
        doAssert(memInst[lsqTag].mem_func == Ld, "only issue Ld");
        doAssert(state[lsqTag][state_issue_port] == Idle, "issuing Ld must be idle");
        doAssert(
            !isValid(ldDepLSQDeq[lsqTag][depLSQDeq_issue_port]) &&
            !isValid(ldDepLdEx[lsqTag][depLdEx_issue_port]) &&
            !isValid(ldDepSB[lsqTag][depSB_issue_port]),
            "issuing entry should not have dependence"
        );
        doAssert(!waitWPResp[lsqTag], "issuing Ld cannot wait for WP resp");
        doAssert(!ldKilled[lsqTag][killed_issue_port], "issuing Ld cannot be killed");
        doAssert(!isMMIO[lsqTag][mmio_issue_port], "issuing Ld cannot be MMIO");

        // current deq ptr
        LdStQTag curDeqP = deqP[deqP_issue_port];

        // index vec for vector functions
        Vector#(LdStQSize, LdStQTag) idxVec = genWith(fromInteger);

        // search for (youngest) older access such that
        // (1) the entry is valid
        // (2) entry is in either case
        //     a. Reconcile
        //     b. computed and same aligned addr mem inst: Lr,Sc,Amo,St and Idle Ld
        // XXX The load must stall on unissued access for the same aligned addr
        // otherwise the previous killing scheme will go wrong
        function Bool matchEntry(LdStQTag i);
            return valid[i][valid_issue_port] && ( // valid entry
                hasReconcile(i) || // reconcile: 
                computed[i][comp_issue_port] && // computed: must be Ld/Lr/St/Sc/Amo, XXX cannot be fence
                sameAlignedAddr(pa, paddr[i][paddr_issue_port]) && // same aligned addr
                (memInst[i].mem_func != Ld || state[i][state_issue_port] == Idle) // Idle load or St/Lr/Sc/Amo
            );
        endfunction
        Vector#(LdStQSize, Bool) isMatch = map(matchEntry, idxVec);

        // search for matching entry
        Maybe#(LdStQTag) matchTag = Invalid;
`ifdef LSQ_VTAG
        // use virtual tag: consider entries older than lsqTag (i.e. smaller tag)
        // note that LSQ must be non-empty, and we search for valid entry
        LSQVTag virtualLsqTag = virtualTags[lsqTag];
        function Bool matchOlderEntry(LdStQTag i);
            return isMatch[i] && virtualTags[i] < virtualLsqTag;
        endfunction
        // get the youngest
        matchTag = findYoungest(map(matchOlderEntry, idxVec));
`else
        // normal search: two cases (LSQ must be non-empty)
        if(lsqTag >= curDeqP) begin
            // older entries are within [deqP, lsqTag), find the largest tag
            function Bool matchOlderEntry(LdStQTag i);
                return i < lsqTag && i >= curDeqP && isMatch[i];
            endfunction
            // since we find the largest tag, we search reversely
            matchTag = find(matchOlderEntry, reverse(idxVec));
        end
        else begin
            // older entries are within [deqP, LdStQSize) + [0, issueTag)
            // first find the largest tag within [0, issueTag)
            function Bool matchEntryFirst(LdStQTag i);
                return i < lsqTag && isMatch[i];
            endfunction
            Maybe#(LdStQTag) firstTag = find(matchEntryFirst, reverse(idxVec));
            // next find the largest tag within [deqP, LdStQSize)
            function Bool matchEntryNext(LdStQTag i);
                return i >= curDeqP && isMatch[i];
            endfunction
            Maybe#(LdStQTag) nextTag = find(matchEntryNext, reverse(idxVec));
            // merge the results
            matchTag = isValid(firstTag) ? firstTag : nextTag;
        end
`endif

        // update state and ldDep & get issue result
        LSQIssueResult issRes = Stall;
        if(matchTag matches tagged Valid .t) begin
            if(hasReconcile(t)) begin
                // wait for reconcile to deq from LSQ
                issRes = Stall;
                ldDepLSQDeq[lsqTag][depLSQDeq_issue_port] <= Valid (t);
            end
            else begin
                case(memInst[t].mem_func)
                    Ld: begin
                        // wait for Ld to issue
                        issRes = Stall;
                        ldDepLdEx[lsqTag][depLdEx_issue_port] <= Valid (t);
                    end
                    Lr, Sc, Amo: begin
                        // wait for Lr/Sc/Amo to deq from LSQ
                        issRes = Stall;
                        ldDepLSQDeq[lsqTag][depLSQDeq_issue_port] <= Valid (t);
                    end
                    St: begin
                        // matches a St, check whether we can bypass data
                        if((pack(shiftedBE[t][shBE_issue_port]) & pack(shift_be)) == pack(shift_be)) begin
                            // matching store covers the issuing load, could forward
                            issRes = Forward (LSQForwardResult {
                                dst: dst[lsqTag],
                                data: shiftedData[t][shData_issue_port]
                            });
                            state[lsqTag][state_issue_port] <= Executing;
                        end
                        else begin
                            // cannot cover, wait for store deq from LSQ
                            issRes = Stall;
                            ldDepLSQDeq[lsqTag][depLSQDeq_issue_port] <= Valid (t);
                        end
                    end
                    default: doAssert(False, "cannot be fence");
                endcase
            end
        end
        else begin
            // not matching entry, check SB search result
            if(sbRes.forwardData matches tagged Valid .d) begin
                // get forward from SB
                issRes = Forward (LSQForwardResult {
                    dst: dst[lsqTag],
                    data: d
                });
                state[lsqTag][state_issue_port] <= Executing;
            end
            else if(sbRes.matchIdx matches tagged Valid .idx) begin
                // SB has matching entry, but cannot fully forward, wait for SB deq
                issRes = Stall;
                ldDepSB[lsqTag][depSB_issue_port] <= Valid (idx);
            end
            else begin
                // send to cache
                issRes = ToCache;
                state[lsqTag][state_issue_port] <= Executing;
            end
        end

        // if the Ld is issued, remove dependences on this issue
        if(issRes != Stall) begin
            function Action setReady(LdStQTag i);
            action
                // no need to check valid here, we can write anything to invalid entry
                if(ldDepLdEx[i][depLdEx_issue_port] == Valid (lsqTag)) begin
                    ldDepLdEx[i][depLdEx_issue_port] <= Invalid;
                end
            endaction
            endfunction
            joinActions(map(setReady, idxVec));
        end

        // make conflict with incorrect spec
        wrongSpec_issue_conflict.wset(?);

        return issRes;
    endmethod

    method ActionValue#(LSQIssueInfo) getIssueLd;
        issueQ.deq;
        // reset ldInIssueQ
        let tag = issueQ.first.data.tag;
        ldInIssueQ[tag][inIssQ_issue_port] <= False;
        doAssert(ldInIssueQ[tag][inIssQ_issue_port], "Ld should be in issueQ");
        return issueQ.first.data;
    endmethod

    method ActionValue#(LSQKillInfo) getKillLd;
        killQ.deq;
        let lsqTag = killQ.first.data;
        doAssert(
            valid[lsqTag][valid_getKill_port] &&
            state[lsqTag][state_getKill_port] != Idle &&
            ldKilled[lsqTag][killed_getKill_port] &&
            memInst[lsqTag].mem_func == Ld,
            "killed load must be valid load in Done/Executing with ldKilled set"
        );
        return LSQKillInfo {
            pc: pc[lsqTag],
            inst_tag: instTag[lsqTag],
            specTag: specTag[lsqTag][specTag_getKill_port]
        };
    endmethod

    method ActionValue#(LSQRespLdResult) respLd(LdStQTag t, Data alignedData);
        let res = LSQRespLdResult {
            wrongPath: False,
            dst: Invalid,
            data: ?
        };
        if(waitWPResp[t]) begin
            waitWPResp[t] <= False;
            res.wrongPath = True;
            res.dst = Invalid; // drop wrong path resp
        end
        else begin
            doAssert(valid[t][valid_respLd_port] && memInst[t].mem_func == Ld,
                "valid resp must come for Ld"
            );
            doAssert(state[t][state_respLd_port] == Executing, "must be executing");
            //doAssert(isValid(dst[t]), "load dst should be valid"); // we may load into x0 reg, then dst is Invalid
            // record resp, load is done
            state[t][state_respLd_port] <= Done;
            res.wrongPath = False;
            res.dst = dst[t];
            res.data = gatherLoad(paddr[t][paddr_respLd_port], memInst[t].byteEn, memInst[t].unsignedLd, alignedData);
            shiftedData[t][shData_respLd_port] <= res.data; // record for seting ROB entry for verification packet
        end
        // make conflict with incorrect spec
        wrongSpec_respLd_conflict.wset(?);
        // return
        return res;
    endmethod

    method LSQDeqEntry first if(valid[deqP[deqP_deq_port]][valid_deq_port]);
        LdStQTag curDeqP = deqP[deqP_deq_port];
        return LSQDeqEntry {
            ldstq_tag: deqP[deqP_deq_port],
            inst_tag: instTag[curDeqP],
            mem_inst: memInst[curDeqP],
            dst: dst[curDeqP],
            paddr: paddr[curDeqP][paddr_deq_port],
            isMMIO: isMMIO[curDeqP][mmio_deq_port],
            shiftedBE: shiftedBE[curDeqP][shBE_deq_port],
            shiftedData: shiftedData[curDeqP][shData_deq_port],
            state: state[curDeqP][state_deq_port],
            computed: computed[curDeqP][comp_deq_port],
            ldKilled: ldKilled[curDeqP][killed_deq_port],
            specTag: specTag[curDeqP][specTag_deq_port],
            spec_bits: specBits[curDeqP][sb_deq_port],
            waitWPResp: waitWPResp[curDeqP]
        };
    endmethod

    method Action deq if(valid[deqP[deqP_deq_port]][valid_deq_port]);
        Reg#(LdStQTag) curDeqP = deqP[deqP_deq_port];
        // sanity check
        doAssert(checkAddrAlign(paddr[curDeqP][paddr_deq_port], memInst[curDeqP].byteEn),
            "addr BE not naturally aligned"
        );
        if(memInst[curDeqP].mem_func == Ld || isMMIO[curDeqP][mmio_deq_port]) begin
            doAssert(isValid(specTag[curDeqP][specTag_deq_port]), "Ld or MMIO must have spec tag");
            doAssert(specBits[curDeqP][sb_deq_port] == (1 << validValue(specTag[curDeqP][specTag_deq_port])),
                "Ld or MMIO must be spec under itself"
            );
        end
        else begin
            doAssert(!isValid(specTag[curDeqP][specTag_deq_port]), "non Ld cannot have spec tag");
            doAssert(specBits[curDeqP][sb_deq_port] == 0, "non Ld cannot under speculation");
        end
        case(memInst[curDeqP].mem_func)
            Ld: begin
                doAssert(!waitWPResp[curDeqP] && !ldKilled[curDeqP][killed_deq_port] &&
                         state[curDeqP][state_deq_port] == (isMMIO[curDeqP][mmio_deq_port] ? Idle : Done),
                         "Ld cannot wait for wrong path resp, cannot be killed; MMIO Ld is idle; non-MMIO Ld is done");
            end
            St: begin
                doAssert(state[curDeqP][state_deq_port] == Idle && computed[curDeqP][comp_deq_port],
                    "St must be Idle and computed"
                );
            end
            Lr, Sc, Amo: begin
                doAssert(state[curDeqP][state_deq_port] == Idle && computed[curDeqP][comp_deq_port] && !waitWPResp[curDeqP],
                    "Lr/Sc/Amo is issued by outside after computing, but cannot wait for wong path resp"
                );
            end
            //Fence: begin
            //    doAssert(False, "Fence is not in LSQ for now");
            //end
            default: doAssert(False, "unknown mem_func");
        endcase
        // remove the entry
        valid[curDeqP][valid_deq_port] <= False;
        curDeqP <= getNextPtr(curDeqP);
        // wakeup loads stalled by this entry
        function Action setReady(Integer i);
        action
            // no need to check valid here, we can write anything to invalid entry
            if(ldDepLSQDeq[i][depLSQDeq_deq_port] == Valid (curDeqP)) begin
                ldDepLSQDeq[i][depLSQDeq_deq_port] <= Invalid;
            end
        endaction
        endfunction
        Vector#(LdStQSize, Integer) idxVec = genVector;
        joinActions(map(setReady, idxVec));
        // make conflict with incorrect spec
        wrongSpec_deq_conflict.wset(?);
    endmethod

    method Action wakeupLdStalledBySB(SBIndex sbIdx);
        function Action setReady(Integer i);
        action
            // no need to check valid here, we can write anything to invalid entry
            if(ldDepSB[i][depSB_wakeSB_port] == Valid (sbIdx)) begin
                ldDepSB[i][depSB_wakeSB_port] <= Invalid;
            end
        endaction
        endfunction
        Vector#(LdStQSize, Integer) idxVec = genVector;
        joinActions(map(setReady, idxVec));
        // make conflict with incorrect spec
        wrongSpec_wakeBySB_conflict.wset(?);
    endmethod

    interface SpeculationUpdate specUpdate;
        method Action correctSpeculation(SpecBits mask);
            // clear spec bits for LSQ entries
            function Action correctSpec(Integer i);
            action
                SpecBits sb = specBits[i][sb_correctSpec_port];
                specBits[i][sb_correctSpec_port] <= sb & mask;
            endaction
            endfunction
            Vector#(LdStQSize, Integer) idxVec = genVector;
            joinActions(map(correctSpec, idxVec));
            // clear spec bits for issueQ and killQ
            issueQ.specUpdate.correctSpeculation(mask);
            killQ.specUpdate.correctSpeculation(mask);
        endmethod

        method Action incorrectSpeculation(SpecTag st);
            // idx vec
            Vector#(LdStQSize, LdStQTag) idxVec = genWith(fromInteger);

            // clear wrong path LSQ entries & set wrong path load filter
            function Action incorrectSpec(LdStQTag i);
            action
                if(specBits[i][sb_wrongSpec_port][st] == 1) begin
                    valid[i][valid_wrongSpec_port] <= False;
                    // set wrong path load resp filter
                    if(valid[i][valid_wrongSpec_port] && state[i][state_wrongSpec_port] == Executing) begin
                        waitWPResp[i] <= True;
                        doAssert(memInst[i].mem_func == Ld, "only load resp can be wrong path");
                    end
                end
            endaction
            endfunction
            joinActions(map(incorrectSpec, idxVec));

            // kill entries in issueQ and killQ
            issueQ.specUpdate.incorrectSpeculation(st);
            killQ.specUpdate.incorrectSpeculation(st);

            // change enqP: make valid entries always consecutive
            // search for the oldest **VALID** entry being killed
            function Bool killValid(LdStQTag i);
                return valid[i][valid_wrongSpec_port] && specBits[i][sb_wrongSpec_port][st] == 1;
            endfunction
            Vector#(LdStQSize, Bool) isKillValid = map(killValid, idxVec);
            // start finding the oldest valid entry being killed
            Maybe#(LdStQTag) killTag; 
`ifdef LSQ_VTAG
            // use virtual tag: find the oldest (note that we search for valid entry)
            killTag = findOldest(isKillValid);
`else
            // normal search, do two rounds
            // first search smallest tag among entries >= deqP
            LdStQTag curDeqP = deqP[deqP_wrongSpec_port];
            function Bool killValidFirst(LdStQTag i);
                return i >= curDeqP && isKillValid[i];
            endfunction
            Maybe#(LdStQTag) killFirst = find(killValidFirst, idxVec);
            // next search smallest tag among all entries
            function Bool killValidNext(LdStQTag i) = isKillValid[i];
            Maybe#(LdStQTag) killNext = find(killValidNext, idxVec);
            // merge
            killTag = isValid(killFirst) ? killFirst : killNext;
`endif
            // new enqP should be the oldest killing entry, otherwise remain the same (no entry killed)
            if(killTag matches tagged Valid .t) begin
                enqP <= t;
            end

            // make conflict with others
            wrongSpec_hit_conflict.wset(?);
            wrongSpec_enqIss_conflict.wset(?);
            wrongSpec_enq_conflict.wset(?);
            wrongSpec_update_conflict.wset(?);
            wrongSpec_issue_conflict.wset(?);
            wrongSpec_respLd_conflict.wset(?);
            wrongSpec_deq_conflict.wset(?);
            wrongSpec_wakeBySB_conflict.wset(?);
        endmethod
    endinterface
endmodule
