
// Copyright (c) 2018 Massachusetts Institute of Technology
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
import CCTypes::*;
import Vector::*;
import GetPut::*;
import Assert::*;
import Ehr::*;
import HasSpecBits::*;
import SpecFifo::*;
import StoreBuffer::*;
import Exec::*;
import SplitLSQ::*;

export mkSerialLSQ;

// --- auxiliary types and functions ---
// virtual index: 0 -- (2 * size - 1)
typedef Bit#(TLog#(TMul#(2, LdQSize))) LdQVirTag;
typedef Bit#(TLog#(TMul#(2, StQSize))) StQVirTag;

// get mem func
function LdQMemFunc getLdQMemFunc(MemFunc f);
    return (case(f)
        Ld: (Ld);
        Lr: (Lr);
        default: ?;
    endcase);
endfunction

function StQMemFunc getStQMemFunc(MemFunc f);
    return (case(f)
        St: (St);
        Sc: (Sc);
        Amo: (Amo);
        default: ?;
    endcase);
endfunction

function Bool isLdQMemFunc(MemFunc f);
    return (case(f)
        Ld, Lr: (True);
        default: (False);
    endcase);
endfunction

function Bool isStQMemFunc(MemFunc f);
    return (case(f)
        St, Sc, Amo: (True);
        default: (False);
    endcase);
endfunction

// --- end of auxiliary types and functions ---

(* synthesize *)
module mkSerialLSQ(SplitLSQ);
    // method/rule ordering (same as SplitLSQ)
    // getHit <
    // deqLd <
    // cacheEvict <
    // updateAddr <
    // issueLd, getIssueLd <
    // (wakeupLdStalledBySB (Weak only) CF deqSt) <
    // setAtCommit <
    // respLd <
    // updateData <
    // (enqLd C enqSt) <
    // correctSpec

    Bool verbose = True;

    // we may simplify things in case of single core
    Bool multicore = valueof(CoreNum) > 1;

    // LQ
    // entry valid bits
    Vector#(LdQSize, Ehr#(2, Bool))                 ld_valid           <- replicateM(mkEhr(False));
    // entry contents
    Vector#(LdQSize, Reg#(InstTag))                 ld_instTag         <- replicateM(mkRegU);
    Vector#(LdQSize, Reg#(LdQMemFunc))              ld_memFunc         <- replicateM(mkRegU);
    Vector#(LdQSize, Reg#(Bool))                    ld_unsigned        <- replicateM(mkRegU);
    Vector#(LdQSize, Reg#(ByteEn))                  ld_byteEn          <- replicateM(mkRegU);
    Vector#(LdQSize, Reg#(Bool))                    ld_acq             <- replicateM(mkRegU);
    Vector#(LdQSize, Reg#(Bool))                    ld_rel             <- replicateM(mkRegU);
    Vector#(LdQSize, Reg#(Maybe#(PhyDst)))          ld_dst             <- replicateM(mkRegU);
    Vector#(LdQSize, Ehr#(2, Addr))                 ld_paddr           <- replicateM(mkEhr(?));
    Vector#(LdQSize, Ehr#(2, Bool))                 ld_isMMIO          <- replicateM(mkEhr(?));
    Vector#(LdQSize, Ehr#(2, ByteEn))               ld_shiftedBE       <- replicateM(mkEhr(?));
    Vector#(LdQSize, Ehr#(2, Maybe#(Exception)))    ld_fault           <- replicateM(mkEhr(?));
    Vector#(LdQSize, Ehr#(2, Bool))                 ld_computed        <- replicateM(mkEhr(?));
    Vector#(LdQSize, Ehr#(2, Bool))                 ld_executing       <- replicateM(mkEhr(?));
    Vector#(LdQSize, Ehr#(2, Bool))                 ld_done            <- replicateM(mkEhr(?));
    Vector#(LdQSize, Ehr#(2, Maybe#(StQTag)))       ld_olderSt         <- replicateM(mkEhr(?));
`ifndef TSO_MM
    Vector#(LdQSize, Ehr#(3, Maybe#(SBIndex)))      ld_depSBDeq        <- replicateM(mkEhr(?));
`endif
    Vector#(LdQSize, Ehr#(3, SpecBits))             ld_specBits        <- replicateM(mkEhr(?));
    Vector#(LdQSize, Ehr#(TAdd#(1, SupSize), Bool)) ld_atCommit        <- replicateM(mkEhr(?));
    // wrong-path load filter (must init to all False)
    Vector#(LdQSize, Ehr#(2, Bool))                 ld_waitWPResp      <- replicateM(mkEhr(False));
    // enq/deq ptr
    Reg#(LdQTag)    ld_enqP <- mkReg(0);
    Ehr#(2, LdQTag) ld_deqP <- mkEhr(0);

    // Ports of each EHR used in each method or rule ("write" means the write
    // port is used, "assert" means the port is only used for assert in
    // simulation)
    let ld_valid_wrongSpec = getVEhrPort(ld_valid, 0); // write
    let ld_valid_deqLd     = getVEhrPort(ld_valid, 0); // write
    let ld_valid_evict     = getVEhrPort(ld_valid, 1);
    let ld_valid_updAddr   = getVEhrPort(ld_valid, 1);
    let ld_valid_issue     = getVEhrPort(ld_valid, 1);
    let ld_valid_deqSt     = getVEhrPort(ld_valid, 1);
    let ld_valid_setCom    = getVEhrPort(ld_valid, 1); // assert
    let ld_valid_resp      = getVEhrPort(ld_valid, 1); // assert
    let ld_valid_enq       = getVEhrPort(ld_valid, 1); // write

    let ld_paddr_deqLd   = getVEhrPort(ld_paddr, 0);
    let ld_paddr_evict   = getVEhrPort(ld_paddr, 0);
    let ld_paddr_updAddr = getVEhrPort(ld_paddr, 0); // write
    let ld_paddr_issue   = getVEhrPort(ld_paddr, 1);
    let ld_paddr_resp    = getVEhrPort(ld_paddr, 1);

    let ld_isMMIO_evict   = getVEhrPort(ld_isMMIO, 0); // assert
    let ld_isMMIO_deqLd   = getVEhrPort(ld_isMMIO, 0);
    let ld_isMMIO_updAddr = getVEhrPort(ld_isMMIO, 0); // write
    let ld_isMMIO_issue   = getVEhrPort(ld_isMMIO, 1); // assert

    let ld_shiftedBE_deqLd   = getVEhrPort(ld_shiftedBE, 0);
    let ld_shiftedBE_updAddr = getVEhrPort(ld_shiftedBE, 0); // write
    let ld_shiftedBE_issue   = getVEhrPort(ld_shiftedBE, 1);

    let ld_fault_deqLd   = getVEhrPort(ld_fault, 0);
    let ld_fault_updAddr = getVEhrPort(ld_fault, 0); // write
    let ld_fault_issue   = getVEhrPort(ld_fault, 1); // assert
    let ld_fault_enq     = getVEhrPort(ld_fault, 1); // write

    let ld_computed_deqLd   = getVEhrPort(ld_computed, 0);
    let ld_computed_evict   = getVEhrPort(ld_computed, 0); // assert
    let ld_computed_updAddr = getVEhrPort(ld_computed, 0); // write
    let ld_computed_issue   = getVEhrPort(ld_computed, 1);
    let ld_computed_resp    = getVEhrPort(ld_computed, 1); // assert
    let ld_computed_enq     = getVEhrPort(ld_computed, 1); // write

    let ld_executing_wrongSpec = getVEhrPort(ld_executing, 0);
    let ld_executing_deqLd     = getVEhrPort(ld_executing, 0);
    let ld_executing_evict     = getVEhrPort(ld_executing, 0);
    let ld_executing_updAddr   = getVEhrPort(ld_executing, 0);
    let ld_executing_issue     = getVEhrPort(ld_executing, 0); // write
    let ld_executing_resp      = getVEhrPort(ld_executing, 1); // assert
    let ld_executing_enq       = getVEhrPort(ld_executing, 1); // write

    let ld_done_wrongSpec = getVEhrPort(ld_done, 0);
    let ld_done_deqLd     = getVEhrPort(ld_done, 0);
    let ld_done_updAddr   = getVEhrPort(ld_done, 0); // assert
    let ld_done_issue     = getVEhrPort(ld_done, 0); // assert
    let ld_done_resp      = getVEhrPort(ld_done, 0); // write
    let ld_done_enq       = getVEhrPort(ld_done, 1); // write

    let ld_olderSt_deqLd  = getVEhrPort(ld_olderSt, 0);
    let ld_olderSt_issue  = getVEhrPort(ld_olderSt, 0);
    let ld_olderSt_deqSt  = getVEhrPort(ld_olderSt, 0); // write
    let ld_olderSt_enq    = getVEhrPort(ld_olderSt, 1); // write

`ifndef TSO_MM
    let ld_depSBDeq_findIss = getVEhrPort(ld_depSBDeq, 0);
    let ld_depSBDeq_issue   = getVEhrPort(ld_depSBDeq, 0); // write
    let ld_depSBDeq_enqIss  = getVEhrPort(ld_depSBDeq, 1); // assert
    let ld_depSBDeq_wakeSB  = getVEhrPort(ld_depSBDeq, 1); // write
    let ld_depSBDeq_enq     = getVEhrPort(ld_depSBDeq, 2); // write
`endif

    let ld_specBits_wrongSpec   = getVEhrPort(ld_specBits, 0); // write
    let ld_specBits_deqLd       = getVEhrPort(ld_specBits, 0); // C with wrongSpec
    let ld_specBits_evict       = getVEhrPort(ld_specBits, 0); // C with wrongSpec
    let ld_specBits_updAddr     = getVEhrPort(ld_specBits, 0); // C with wrongSpec
    let ld_specBits_enq         = getVEhrPort(ld_specBits, 0); // write, C with wrongSpec
    let ld_specBits_correctSpec = getVEhrPort(ld_specBits, 1); // write

    let ld_atCommit_wrongSpec = getVEhrPort(ld_atCommit, 0);
    let ld_atCommit_deqLd     = getVEhrPort(ld_atCommit, 0);
    Vector#(SupSize, Vector#(LdQSize, Reg#(Bool))) ld_atCommit_setCom;
    for(Integer i = 0; i < valueof(SupSize); i = i+1) begin
        ld_atCommit_setCom[i] = getVEhrPort(ld_atCommit, i); // write
    end
    let ld_atCommit_enq = getVEhrPort(ld_atCommit, valueof(SupSize)); // write

    let ld_waitWPResp_hit       = getVEhrPort(ld_waitWPResp, 0);
    let ld_waitWPResp_deqLd     = getVEhrPort(ld_waitWPResp, 0); // write
    let ld_waitWPResp_updAddr   = getVEhrPort(ld_waitWPResp, 1);
    let ld_waitWPResp_issue     = getVEhrPort(ld_waitWPResp, 1); // assert
    let ld_waitWPResp_resp      = getVEhrPort(ld_waitWPResp, 1); // write
    let ld_waitWPResp_wrongSpec = getVEhrPort(ld_waitWPResp, 1); // write

    Reg#(LdQTag) ld_deqP_deqLd = ld_deqP[0]; // write
    Reg#(LdQTag) ld_deqP_issue = ld_deqP[1];
    Reg#(LdQTag) ld_deqP_deqSt = ld_deqP[1];

    // SQ
    // entry valid bits
    Vector#(StQSize, Ehr#(2, Bool))                 st_valid     <- replicateM(mkEhr(False));
    // entry contents
    Vector#(StQSize, Reg#(InstTag))                 st_instTag   <- replicateM(mkRegU);
    Vector#(StQSize, Reg#(StQMemFunc))              st_memFunc   <- replicateM(mkRegU);
    Vector#(StQSize, Reg#(AmoFunc))                 st_amoFunc   <- replicateM(mkRegU);
    Vector#(StQSize, Reg#(ByteEn))                  st_byteEn    <- replicateM(mkRegU);
    Vector#(StQSize, Reg#(Bool))                    st_acq       <- replicateM(mkRegU);
    Vector#(StQSize, Reg#(Bool))                    st_rel       <- replicateM(mkRegU);
    Vector#(StQSize, Reg#(Maybe#(PhyDst)))          st_dst       <- replicateM(mkRegU);
    Vector#(StQSize, Ehr#(2, Addr))                 st_paddr     <- replicateM(mkEhr(?));
    Vector#(StQSize, Ehr#(2, Bool))                 st_isMMIO    <- replicateM(mkEhr(?));
    Vector#(StQSize, Ehr#(2, ByteEn))               st_shiftedBE <- replicateM(mkEhr(?));
    Vector#(StQSize, Ehr#(1, Data))                 st_stData    <- replicateM(mkEhr(?));
    Vector#(StQSize, Ehr#(2, Maybe#(Exception)))    st_fault     <- replicateM(mkEhr(?));
    Vector#(StQSize, Ehr#(2, Bool))                 st_computed  <- replicateM(mkEhr(?));
    Vector#(StQSize, Ehr#(2, SpecBits))             st_specBits  <- replicateM(mkEhr(?));
    Vector#(StQSize, Ehr#(TAdd#(1, SupSize), Bool)) st_atCommit  <- replicateM(mkEhr(?));
    // enq/deq ptr
    Reg#(StQTag) st_enqP <- mkReg(0);
    Reg#(StQTag) st_deqP <- mkReg(0);

    let st_valid_empty     = getVEhrPort(st_valid, 0);
    let st_valid_wrongSpec = getVEhrPort(st_valid, 0); // write
    let st_valid_updAddr   = getVEhrPort(st_valid, 0); // assert
    let st_valid_deqSt     = getVEhrPort(st_valid, 0); // write
    let st_valid_setCom    = getVEhrPort(st_valid, 1); // assert
    let st_valid_updData   = getVEhrPort(st_valid, 1); // assert
    let st_valid_enq       = getVEhrPort(st_valid, 1); // write

    let st_paddr_updAddr = getVEhrPort(st_paddr, 0); // write
    let st_paddr_deqSt   = getVEhrPort(st_paddr, 1);

    let st_isMMIO_updAddr = getVEhrPort(st_isMMIO, 0); // write
    let st_isMMIO_deqSt   = getVEhrPort(st_isMMIO, 1);

    let st_shiftedBE_updAddr = getVEhrPort(st_shiftedBE, 0); // write
    let st_shiftedBE_deqSt   = getVEhrPort(st_shiftedBE, 1);
    
    let st_stData_deqSt   = getVEhrPort(st_stData, 0);
    let st_stData_updData = getVEhrPort(st_stData, 0); // write

    let st_fault_updAddr = getVEhrPort(st_fault, 0); // write
    let st_fault_deqSt   = getVEhrPort(st_fault, 1);
    let st_fault_enq     = getVEhrPort(st_fault, 1); // write

    let st_computed_updAddr = getVEhrPort(st_computed, 0); // write
    let st_computed_deqSt   = getVEhrPort(st_computed, 1);
    let st_computed_updData = getVEhrPort(st_computed, 1); // assert
    let st_computed_enq     = getVEhrPort(st_computed, 1); // write

    let st_specBits_wrongSpec   = getVEhrPort(st_specBits, 0); // write
    let st_specBits_updAddr     = getVEhrPort(st_specBits, 0); // C with wrongSpec
    let st_specBits_deqSt       = getVEhrPort(st_specBits, 0); // C with wrongSpec
    let st_specBits_enq         = getVEhrPort(st_specBits, 0); // write, C with wrongSpec
    let st_specBits_correctSpec = getVEhrPort(st_specBits, 1); // write

    let st_atCommit_wrongSpec = getVEhrPort(st_atCommit, 0);
    let st_atCommit_deqSt     = getVEhrPort(st_atCommit, 0);
    Vector#(SupSize, Vector#(StQSize, Reg#(Bool))) st_atCommit_setCom;
    for(Integer i = 0; i < valueof(SupSize); i = i+1) begin
        st_atCommit_setCom[i] = getVEhrPort(st_atCommit, i); // write
    end
    let st_atCommit_enq = getVEhrPort(st_atCommit, valueof(SupSize));

    // make wrongSpec conflict with all others (but not correctSpec method and
    // findIssue)
    RWire#(void) wrongSpec_hit_conflict <- mkRWire;
    RWire#(void) wrongSpec_enqIss_conflict <- mkRWire;
    RWire#(void) wrongSpec_enq_conflict <- mkRWire;
    RWire#(void) wrongSpec_cacheEvict_conflict <- mkRWire;
    RWire#(void) wrongSpec_update_conflict <- mkRWire;
    RWire#(void) wrongSpec_issue_conflict <- mkRWire;
    RWire#(void) wrongSpec_respLd_conflict <- mkRWire;
    RWire#(void) wrongSpec_deqLd_conflict <- mkRWire;
    RWire#(void) wrongSpec_deqSt_conflict <- mkRWire;
    RWire#(void) wrongSpec_verify_conflict <- mkRWire;
    RWire#(void) wrongSpec_wakeBySB_conflict <- mkRWire;
    // make wrongSpec more urgent than firstSt (resolve bsc error)
    Wire#(Bool) wrongSpec_urgent_firstSt <- mkDWire(True);

    function LdQTag getNextLdPtr(LdQTag t);
        return t == fromInteger(valueOf(LdQSize) - 1) ? 0 : t + 1;
    endfunction

    function StQTag getNextStPtr(StQTag t);
        return t == fromInteger(valueOf(StQSize) - 1) ? 0 : t + 1;
    endfunction

    // Virtual tag
    // Since enqP is not changed during all our associative searches, we map
    // LQ/SQ index to virtual tags using enqP as pivot.
    // The mapping is as follow:
    // - valid entry i --> i < enqP ? i + QSize : i
    // XXX This mapping is only for comparing valid entries (e.g., it may not
    // work properly for enqP), so we must check entry valid before using
    // virtual tags or do special calculation.
    function LdQVirTag getLdVirTag(LdQTag i);
        return i < ld_enqP ? zeroExtend(i) + fromInteger(valueof(LdQSize))
                           : zeroExtend(i);
    endfunction
    function StQVirTag getStVirTag(StQTag i);
        return i < st_enqP ? zeroExtend(i) + fromInteger(valueof(StQSize))
                           : zeroExtend(i);
    endfunction
    // virtual tags for LQ/SQ indices to be reused in all associative searches
    Vector#(LdQSize, LdQVirTag) ldVirTags = map(getLdVirTag,
                                                genWith(fromInteger));
    Vector#(StQSize, StQVirTag) stVirTags = map(getStVirTag,
                                                genWith(fromInteger));

    // find oldest LQ entry that satisfy a constraint (i.e. smallest tag)
    function Maybe#(LdQTag) findOldestLd(Vector#(LdQSize, Bool) pred);
        function LdQTag getOlder(LdQTag a, LdQTag b);
            if(!pred[a]) begin
                return b;
            end
            else if(!pred[b]) begin
                return a;
            end
            else begin
                return ldVirTags[a] < ldVirTags[b] ? a : b;
            end
        endfunction
        Vector#(LdQSize, LdQTag) idxVec = genWith(fromInteger);
        LdQTag tag = fold(getOlder, idxVec);
        return pred[tag] ? Valid (tag) : Invalid;
    endfunction

    // find oldest SQ entry that satisfy a constraint (i.e. smallest tag)
    function Maybe#(StQTag) findOldestSt(Vector#(StQSize, Bool) pred);
        function StQTag getOlder(StQTag a, StQTag b);
            if(!pred[a]) begin
                return b;
            end
            else if(!pred[b]) begin
                return a;
            end
            else begin
                return stVirTags[a] < stVirTags[b] ? a : b;
            end
        endfunction
        Vector#(StQSize, StQTag) idxVec = genWith(fromInteger);
        StQTag tag = fold(getOlder, idxVec);
        return pred[tag] ? Valid (tag) : Invalid;
    endfunction

`ifdef BSIM
    // Sanity check in simulation. All valid entry are consective within deqP
    // and enqP, outsiders are invalid entries
    (* fire_when_enabled, no_implicit_conditions *)
    rule checkLdQValid;
        if(all(\== (False),  readVEhr(0, ld_valid))) begin
            doAssert(ld_enqP == ld_deqP[0], "empty queue have enqP = deqP");
        end
        else begin
            // not empty queue, check valid entries with [deqP, enqP)
            function Bool in_range(LdQTag i);
                if(ld_deqP[0] < ld_enqP) begin
                    return ld_deqP[0] <= i && i < ld_enqP;
                end
                else begin
                    return ld_deqP[0] <= i || i < ld_enqP;
                end
            endfunction
            for(Integer i = 0; i < valueof(LdQSize); i = i+1) begin
                doAssert(in_range(fromInteger(i)) == ld_valid[i][0],
                        "valid entries must be within [deqP, enqP)");
            end
        end
    endrule

    (* fire_when_enabled, no_implicit_conditions *)
    rule checkStQValid;
        if(all(\== (False), readVEhr(0, st_valid))) begin
            doAssert(st_deqP == st_enqP, "empty queue have enqP = deqP");
        end
        else begin
            // not empty queue, check valid entries with [deqP, enqP)
            function Bool in_range(StQTag i);
                if(st_deqP < st_enqP) begin
                    return st_deqP <= i && i < st_enqP;
                end
                else begin
                    return st_deqP <= i || i < st_enqP;
                end
            endfunction
            for(Integer i = 0; i < valueof(StQSize); i = i+1) begin
                doAssert(in_range(fromInteger(i)) == st_valid[i][0],
                         "valid entries must be within [deqP, enqP)");
            end
        end
    endrule
`endif

    // lazy enq guard signal
    Wire#(Bool) ld_can_enq_wire <- mkBypassWire;
    Wire#(Bool) st_can_enq_wire <- mkBypassWire;
    (* fire_when_enabled, no_implicit_conditions *)
    rule setForEnq;
        ld_can_enq_wire <= !ld_valid[ld_enqP][0];
        st_can_enq_wire <= !st_valid[st_enqP][0];
    endrule

    // deqLd guard (see comments at the method declaration)
    function Bool deqLdGuard;
        LdQTag deqP = ld_deqP_deqLd;
        if(!ld_valid_deqLd[deqP]) begin
            return False; // not valid
        end
        else begin
            if(isValid(ld_fault_deqLd[deqP])) begin
                return True; // fault
            end
            else begin
                Bool no_older_st = !isValid(ld_olderSt_deqLd[deqP]);
                if(ld_memFunc[deqP] == Ld && !ld_isMMIO_deqLd[deqP]) begin
                    // normal non-MMIO Ld: done, no older St
                    return ld_done_deqLd[deqP] && no_older_st;
                end
                else begin
                    // Lr or MMIO: done, at commit, no older st
                    return ld_computed_deqLd[deqP] &&
                           ld_atCommit_deqLd[deqP] &&
                           no_older_st;
                end
            end
        end
    endfunction

    // deqSt guard (see comments at the method declaration)
    function Bool deqStGuard;
        StQTag deqP = st_deqP;
        if(!st_valid_deqSt[deqP]) begin
            return False;
        end
        else begin
            if(isValid(st_fault_deqSt[deqP])) begin
                return True; // fault
            end
            else begin
                // computed and at commit
                return st_computed_deqSt[deqP] && st_atCommit_deqSt[deqP];
            end
        end
    endfunction

    Vector#(SupSize, Put#(LdStQTag)) setAtCommitIfc;
    for(Integer i = 0; i < valueof(SupSize); i = i+1) begin
        setAtCommitIfc[i] = (interface Put;
            method Action put(LdStQTag lsqTag);
                case(lsqTag) matches
                    tagged Ld .tag: begin
                        ld_atCommit_setCom[i][tag] <= True;
                        doAssert(ld_valid_setCom[tag], "must be valid");
                    end
                    tagged St .tag: begin
                        st_atCommit_setCom[i][tag] <= True;
                        doAssert(st_valid_setCom[tag], "must be valid");
                    end
                    default: doAssert(False, "unknown lsq tag");
                endcase
            endmethod
        endinterface);
    end

    method ByteEn getOrigBE(LdStQTag t);
        return (case(t) matches
            tagged Ld .tag: (ld_byteEn[tag]);
            tagged St .tag: (st_byteEn[tag]);
            default: ?;
        endcase);
    endmethod

    method ActionValue#(LSQHitInfo) getHit(LdStQTag t);
        // Conflict with wrong spec. This makes cache pipelineResp rule
        // conflict with wrong spec, and can help avoid scheduling cycle.
        wrongSpec_hit_conflict.wset(?);
        return (case(t) matches
            tagged Ld .tag: (LSQHitInfo {
                waitWPResp: ld_waitWPResp_hit[tag],
                dst: ld_dst[tag]
            });
            tagged St .tag: (LSQHitInfo {
                waitWPResp: False,
                dst: st_dst[tag]
            });
            default: ?;
        endcase);
    endmethod

    method Maybe#(LdStQTag) enqLdTag;
        return ld_can_enq_wire ? Valid (Ld (ld_enqP)) : Invalid;
    endmethod
    method Maybe#(LdStQTag) enqStTag;
        return st_can_enq_wire ? Valid (St (st_enqP)) : Invalid;
    endmethod

    method Action enqLd(InstTag inst_tag,
                        MemInst mem_inst,
                        Maybe#(PhyDst) dst,
                        SpecBits spec_bits) if(ld_can_enq_wire);
        if(verbose) begin
            $display("[LSQ - enqLd] enqP %d; ", ld_enqP,
                     "; ", fshow(inst_tag),
                     "; ", fshow(mem_inst),
                     "; ", fshow(dst),
                     "; ", fshow(spec_bits));
        end
        doAssert(!ld_valid_enq[ld_enqP],
                 "entry at enqP must be invalid");
        doAssert(isLdQMemFunc(mem_inst.mem_func),
                 "must be LdQ mem func");
        // set entry valid and move ptr
        ld_valid_enq[ld_enqP] <= True;
        ld_enqP <= getNextLdPtr(ld_enqP);
        // set up most of the entry
        ld_instTag[ld_enqP] <= inst_tag;
        ld_memFunc[ld_enqP] <= getLdQMemFunc(mem_inst.mem_func);
        ld_unsigned[ld_enqP] <= mem_inst.unsignedLd;
        ld_byteEn[ld_enqP] <= mem_inst.byteEn;
        ld_acq[ld_enqP] <= mem_inst.aq;
        ld_rel[ld_enqP] <= mem_inst.rl;
        ld_dst[ld_enqP] <= dst;
        ld_fault_enq[ld_enqP] <= Invalid;
        ld_computed_enq[ld_enqP] <= False;
        ld_executing_enq[ld_enqP] <= False;
        ld_done_enq[ld_enqP] <= False;
`ifndef TSO_MM
        ld_depSBDeq_enq[ld_enqP] <= Invalid;
`endif
        ld_specBits_enq[ld_enqP] <= spec_bits;
        ld_atCommit_enq[ld_enqP] <= False;
        // don't touch wait wrong resp
        // Record older St. XXX We must use the up-to-date value st_valid;
        // otherwise, we may record a valid olderSt and never get it reset.
        StQTag olderSt = st_enqP == 0 ? fromInteger(valueof(StQSize) - 1)
                                      : (st_enqP - 1);
        if(st_valid_enq[olderSt]) begin
            ld_olderSt_enq[ld_enqP] <= Valid (olderSt);
        end
        else begin
            ld_olderSt_enq[ld_enqP] <= Invalid;
        end
        // make conflict with incorrect spec
        wrongSpec_enq_conflict.wset(?);
    endmethod

    method Action enqSt(InstTag inst_tag,
                        MemInst mem_inst,
                        Maybe#(PhyDst) dst,
                        SpecBits spec_bits) if(st_can_enq_wire);
        if(verbose) begin
            $display("[LSQ - enqSt] enqP %d; ", st_enqP,
                     "; ", fshow(inst_tag),
                     "; ", fshow(mem_inst),
                     "; ", fshow(dst),
                     "; ", fshow(spec_bits));
        end
        doAssert(!st_valid_enq[st_enqP],
                 "entry at enqP must be invalid");
        doAssert(isStQMemFunc(mem_inst.mem_func),
                 "must be StQ mem func");
        // set entry valid and move ptr
        st_valid_enq[st_enqP] <= True;
        st_enqP <= getNextStPtr(st_enqP);
        // set up the entry
        st_instTag[st_enqP] <= inst_tag;
        st_memFunc[st_enqP] <= getStQMemFunc(mem_inst.mem_func);
        st_amoFunc[st_enqP] <= mem_inst.amo_func;
        st_byteEn[st_enqP] <= mem_inst.byteEn;
        st_acq[st_enqP] <= mem_inst.aq;
        st_rel[st_enqP] <= mem_inst.rl;
        st_dst[st_enqP] <= dst;
        st_fault_enq[st_enqP] <= Invalid;
        st_computed_enq[st_enqP] <= False;
        st_specBits_enq[st_enqP] <= spec_bits;
        st_atCommit_enq[st_enqP] <= False;
        // make conflict with incorrect spec
        wrongSpec_enq_conflict.wset(?);
    endmethod

    method Action updateData(StQTag t, Data d);
        doAssert(st_valid_updData[t], "entry must be valid");
        doAssert(!st_computed_updData[t], "entry cannot be computed");
        st_stData_updData[t] <= d;
    endmethod

    method ActionValue#(LSQUpdateAddrResult) updateAddr(
        LdStQTag lsqTag, Maybe#(Exception) fault,
        Addr pa, Bool mmio, ByteEn shift_be
    );
        // update LQ/SQ entry and prepare for killing loads
        if(lsqTag matches tagged Ld .tag) begin
            // sanity check
            doAssert(ld_valid_updAddr[tag],
                     "updating entry must be valid");
            doAssert(!ld_computed_updAddr[tag] &&
                     !ld_executing_updAddr[tag] &&
                     !ld_done_updAddr[tag],
                     "updating entry should not be " +
                     "computed or issuing or executed or done or killed");

            // write fault, computed paddr, shift be. NOTE computed is
            // true only when no fault.
            ld_fault_updAddr[tag] <= fault;
            ld_computed_updAddr[tag] <= !isValid(fault);
            ld_paddr_updAddr[tag] <= pa;
            ld_isMMIO_updAddr[tag] <= mmio;
            ld_shiftedBE_updAddr[tag] <= shift_be;
        end
        else if(lsqTag matches tagged St .tag) begin
            // sanity check
            doAssert(st_valid_updAddr[tag],
                     "updating entry must be valid");
            doAssert(!st_computed_updAddr[tag],
                     "updating entry should not be computed");

            // write fault, computed paddr, shift be. NOTE computed is
            // true only when no fault.
            st_fault_updAddr[tag] <= fault;
            st_computed_updAddr[tag] <= !isValid(fault);
            st_paddr_updAddr[tag] <= pa;
            st_isMMIO_updAddr[tag] <= mmio;
            st_shiftedBE_updAddr[tag] <= shift_be;
        end
        else begin
            doAssert(False, "unknown lsq tag");
        end

        if(verbose) begin
            $display("[LSQ - updateAddr] ", fshow(lsqTag), "; ", fshow(fault),
                     "; ", fshow(pa), "; ", fshow(mmio), "; ", fshow(shift_be));
        end

        // make conflict with incorrect spec
        wrongSpec_update_conflict.wset(?);

        // return waiting for wp resp bit: for deciding whether the updating Ld
        // can be issued. Here we only issue load when it is the oldest in LSQ,
        // so always return True here to prevent issue
        return LSQUpdateAddrResult {
            waitWPResp: (case(lsqTag) matches
                tagged Ld .tag: True;
                default: False;
            endcase)
        };
    endmethod

    method ActionValue#(LSQIssueLdResult) issueLd(LdQTag tag,
                                                  Addr pa,
                                                  ByteEn shift_be,
                                                  SBSearchRes sbRes);
        if(verbose) begin
            $display("[LSQ - issueLd] ", fshow(tag), "; ", fshow(pa),
                     "; ", fshow(shift_be), "; ", fshow(sbRes));
        end
        doAssert(pa == ld_paddr_issue[tag], "Ld paddr incorrect");
        doAssert(shift_be == ld_shiftedBE_issue[tag], "Ld BE incorrect");
        doAssert(ld_valid_issue[tag], "issuing Ld must be valid");
        doAssert(!isValid(ld_fault_issue[tag]), "issuing Ld cannot be fault");
        doAssert(ld_computed_issue[tag], "issuing Ld must be computed");
        doAssert(!ld_executing_issue[tag], "issuing Ld must not be executing");
        doAssert(!ld_done_issue[tag], "issuing Ld must not be done");
        doAssert(ld_memFunc[tag] == Ld, "only issue Ld");
        doAssert(!ld_isMMIO_issue[tag], "issuing Ld cannot be MMIO");
`ifndef TSO_MM
        doAssert(
            !isValid(ld_depSBDeq_issue[tag]),
            "issuing entry should not have dependence"
        );
`endif
        doAssert(!ld_waitWPResp_issue[tag], "issuing Ld cannot wait for WP resp");

        // issue load should be the oldest in LSQ, 
        doAssert(tag == ld_deqP_issue, "must be oldest load");
        doAssert(!isValid(ld_olderSt_issue[tag]), "cannot have older store");

        // issue result
        LSQIssueLdResult issRes = Stall;

`ifdef TSO_MM
        // always send to cache, set executing
        issRes = ToCache;
        ld_executing_issue[tag] <= True;
`else
        // just check SB search result
        if(sbRes.forwardData matches tagged Valid .d) begin
            // get forward from SB
            issRes = Forward (LSQForwardResult {
                dst: ld_dst[tag],
                data: d
            });
            // set executing
            ld_executing_issue[tag] <= True;
        end
        else if(sbRes.matchIdx matches tagged Valid .idx) begin
            // SB has matching entry, but cannot fully forward, wait for SB
            // deq
            issRes = Stall;
            ld_depSBDeq_issue[tag] <= Valid (idx);
        end
        else begin
            // send to cache
            issRes = ToCache;
            ld_executing_issue[tag] <= True;
        end
`endif

        // make conflict with incorrect spec
        wrongSpec_issue_conflict.wset(?);

        return issRes;
    endmethod

    method ActionValue#(LSQIssueLdInfo) getIssueLd;
        LdQTag tag = ld_deqP_issue;
        // issue oldest load when
        // (1) valid
        // (2) computed
        // (3) not executing
        // (4) no depend
        // (5) Ld
        // (6) not MMIO
        // (7) not wait for wrong path
        when(ld_valid_issue[tag] &&
             !isValid(ld_olderSt_issue[tag]) &&
             ld_computed_issue[tag] &&
             !ld_executing_issue[tag] &&
`ifndef TSO_MM 
             !isValid(ld_depSBDeq_issue[tag]) &&
`endif
             ld_memFunc[tag] == Ld &&
             !ld_isMMIO_issue[tag] &&
             !ld_waitWPResp_issue[tag], noAction);
        let info = LSQIssueLdInfo {
            tag: tag,
            paddr: ld_paddr_issue[tag],
            shiftedBE: ld_shiftedBE_issue[tag]
        };
        if(verbose) begin
            $display("[LSQ - getIssueLd] ", fshow(info));
        end
        return info;
    endmethod

    method ActionValue#(LSQRespLdResult) respLd(LdQTag t, Data alignedData);
        let res = LSQRespLdResult {
            wrongPath: False,
            dst: Invalid,
            data: ?
        };
        if(ld_waitWPResp_resp[t]) begin
            ld_waitWPResp_resp[t] <= False;
            res.wrongPath = True;
            res.dst = Invalid; // drop wrong path resp
        end
        else begin
            doAssert(ld_valid_resp[t] && ld_memFunc[t] == Ld,
                     "valid resp must come for Ld");
            doAssert(ld_computed_resp[t] &&
                     ld_executing_resp[t] &&
                     !ld_done_resp[t],
                     "must be computed, executing, not done");
            // we may load into x0 reg, then dst is Invalid, so don't assert
            // that dst is valid
            // mark load as done, and shift resp
            ld_done_resp[t] <= True;
            res.wrongPath = False;
            res.dst = ld_dst[t];
            res.data = gatherLoad(ld_paddr_resp[t], ld_byteEn[t],
                                  ld_unsigned[t], alignedData);
        end
        if(verbose) begin
            $display("[LSQ - respLd] ", fshow(t), "; ", fshow(alignedData),
                     "; ", fshow(res));
        end
        // make conflict with incorrect spec
        wrongSpec_respLd_conflict.wset(?);
        // return
        return res;
    endmethod

    method LdQDeqEntry firstLd if(deqLdGuard);
        LdQTag deqP = ld_deqP_deqLd;
        return LdQDeqEntry {
            instTag: ld_instTag[deqP],
            memFunc: ld_memFunc[deqP],
            byteEn: ld_byteEn[deqP],
            unsignedLd: ld_unsigned[deqP],
            rel: ld_rel[deqP],
            dst: ld_dst[deqP],
            paddr: ld_paddr_deqLd[deqP],
            isMMIO: ld_isMMIO_deqLd[deqP],
            shiftedBE: ld_shiftedBE_deqLd[deqP],
            fault: ld_fault_deqLd[deqP],
            killed: False // never killed
        };
    endmethod

    method Action deqLd if(deqLdGuard);
        LdQTag deqP = ld_deqP_deqLd;

        if(verbose) $display("[LSQ - deqLd] deqP %d", deqP);

        // sanity check
        if(ld_atCommit_deqLd[deqP]) begin
            doAssert(ld_specBits_deqLd[deqP] == 0,
                     "at commit means zero spec bits");
        end

        // remove the entry
        ld_valid_deqLd[deqP] <= False;
        ld_deqP_deqLd <= getNextLdPtr(deqP);

        // make conflict with incorrect spec
        wrongSpec_deqLd_conflict.wset(?);
    endmethod

    method StQDeqEntry firstSt if(deqStGuard && wrongSpec_urgent_firstSt);
        StQTag deqP = st_deqP;
        return StQDeqEntry {
            instTag: st_instTag[deqP],
            memFunc: st_memFunc[deqP],
            amoFunc: st_amoFunc[deqP],
            acq: st_acq[deqP],
            rel: st_rel[deqP],
            dst: st_dst[deqP],
            paddr: st_paddr_deqSt[deqP],
            isMMIO: st_isMMIO_deqSt[deqP],
            shiftedBE: st_shiftedBE_deqSt[deqP],
            stData: st_stData_deqSt[deqP],
            fault: st_fault_deqSt[deqP]
        };
    endmethod

    method Action deqSt if(deqStGuard);
        StQTag deqP = st_deqP;

        if(verbose) $display("[LSQ - deqSt] deqP %d", deqP);

        // sanity check
        if(!isValid(st_fault_deqSt[deqP])) begin
            doAssert(st_specBits_deqSt[deqP] == 0,
                     "must have zero spec bits");
        end

        // remove entry
        st_valid_deqSt[deqP] <= False;
        let new_st_deqP = getNextStPtr(deqP);
        st_deqP <= new_st_deqP;

        // tell LQ entries that this SQ entry is removed, no need to check LQ
        // entry valid:
        // (1) reset olderSt
        function Action resetSt(LdQTag i);
        action
            if(ld_olderSt_deqSt[i] == Valid (deqP)) begin
                ld_olderSt_deqSt[i] <= Invalid;
                // no need to change ld_olderStVerified, it is only meaningful
                // when ld_olderSt is valid
            end
        endaction
        endfunction
        Vector#(LdQSize, LdQTag) idxVec = genWith(fromInteger);
        joinActions(map(resetSt, idxVec));

        // make conflict with incorrect spec
        wrongSpec_deqSt_conflict.wset(?);
    endmethod

`ifdef TSO_MM
    method Action cacheEvict(LineAddr lineAddr);
        // make conflict with incorrect spec
        wrongSpec_cacheEvict_conflict.wset(?);
    endmethod

`else

    method Action wakeupLdStalledBySB(SBIndex sbIdx);
        if(verbose) begin
            $display("[LSQ - wakeupBySB] ", fshow(sbIdx));
        end
        function Action setReady(LdQTag i);
        action
            // no need to check valid here, we can write anything to invalid
            // entry
            if(ld_depSBDeq_wakeSB[i] == Valid (sbIdx)) begin
                ld_depSBDeq_wakeSB[i] <= Invalid;
            end
        endaction
        endfunction
        Vector#(LdQSize, LdQTag) idxVec = genWith(fromInteger);
        joinActions(map(setReady, idxVec));
        // make conflict with incorrect spec
        wrongSpec_wakeBySB_conflict.wset(?);
    endmethod
`endif

    method Bool stqEmpty;
        return !st_valid_empty[st_deqP];
    endmethod

    interface setAtCommit = setAtCommitIfc;

    interface SpeculationUpdate specUpdate;
        method Action correctSpeculation(SpecBits mask);
            if(verbose && mask != maxBound) begin
                $display("[LSQ - correctSpec] ", fshow(mask));
            end
            // clear spec bits for LQ entries
            function Action correctSpecLd(LdQTag i);
            action
                SpecBits sb = ld_specBits_correctSpec[i];
                ld_specBits_correctSpec[i] <= sb & mask;
            endaction
            endfunction
            Vector#(LdQSize, LdQTag) ldIdxVec = genWith(fromInteger);
            joinActions(map(correctSpecLd, ldIdxVec));

            // clear spec bits for LQ entries
            function Action correctSpecSt(StQTag i);
            action
                SpecBits sb = st_specBits_correctSpec[i];
                st_specBits_correctSpec[i] <= sb & mask;
            endaction
            endfunction
            Vector#(StQSize, StQTag) stIdxVec = genWith(fromInteger);
            joinActions(map(correctSpecSt, stIdxVec));
        endmethod

        method Action incorrectSpeculation(Bool killAll, SpecTag specTag);
            // idx vec

            // clear wrong path LQ entries & set wrong path load filter. NOTE
            // that olderSt and olderStVerified fields are not affected by the
            // kill. NOTE that if atCommit is true, then killAll should not
            // kill this entry (for SQ)
            Vector#(LdQSize, LdQTag) ldIdxVec = genWith(fromInteger);
            function Bool isLdNeedKill(LdQTag i);
                return (killAll ? !ld_atCommit_wrongSpec[i] :
                        ld_specBits_wrongSpec[i][specTag] == 1);
            endfunction
            Vector#(LdQSize, Bool) ldNeedKill = map(isLdNeedKill, ldIdxVec);
            function Action killLdQ(LdQTag i);
            action
                if(ldNeedKill[i]) begin
                    ld_valid_wrongSpec[i] <= False;
                    // set wrong path load resp filter
                    if (ld_valid_wrongSpec[i] &&
                        ld_executing_wrongSpec[i] &&
                        !ld_done_wrongSpec[i]) begin
                        ld_waitWPResp_wrongSpec[i] <= True;
                        doAssert(ld_memFunc[i] == Ld,
                                 "only load resp can be wrong path");
                    end
                    doAssert(!ld_atCommit_wrongSpec[i], "cannot be at commit");
                end
            endaction
            endfunction
            joinActions(map(killLdQ, ldIdxVec));

            // clear wrong path SQ entries. NOTE that inst at commit should not
            // be killed in case of kill all
            Vector#(StQSize, StQTag) stIdxVec = genWith(fromInteger);
            function Bool isStNeedKill(StQTag i);
                return (killAll ? !st_atCommit_wrongSpec[i] :
                                  st_specBits_wrongSpec[i][specTag] == 1);
            endfunction
            Vector#(StQSize, Bool) stNeedKill = map(isStNeedKill, stIdxVec);
            function Action killStQ(StQTag i);
            action
                if(stNeedKill[i]) begin
                    st_valid_wrongSpec[i] <= False;
                    doAssert(!st_atCommit_wrongSpec[i], "cannot be at commit");
                end
            endaction
            endfunction
            joinActions(map(killStQ, stIdxVec));

            // change enqP: make valid entries always consecutive: new enqP is
            // the oldest **VALID** entry that gets killed. If such entry does
            // not exists, then enqP remains the same.
            // LdQ enqP
            function Bool isValidLdKilled(LdQTag i);
                return ld_valid_wrongSpec[i] && ldNeedKill[i];
            endfunction
            Vector#(LdQSize, Bool) killedValidLds = map(isValidLdKilled,
                                                        ldIdxVec);
            LdQTag new_ld_enqP = ld_enqP;
            if(findOldestLd(killedValidLds) matches tagged Valid .t) begin
                new_ld_enqP = t;
            end
            ld_enqP <= new_ld_enqP;

            // StQ enqP
            function Bool isValidStKilled(StQTag i);
                return st_valid_wrongSpec[i] && stNeedKill[i];
            endfunction
            Vector#(StQSize, Bool) killedValidSts = map(isValidStKilled,
                                                        stIdxVec);
            StQTag new_st_enqP = st_enqP;
            if(findOldestSt(killedValidSts) matches tagged Valid .t) begin
                new_st_enqP = t;
            end
            st_enqP <= new_st_enqP;

            if(verbose) begin
                $display("[LSQ - wrongSpec] ", fshow(killAll),
                         "; ", fshow(specTag),
                         "; ", fshow(new_ld_enqP),
                         "; ", fshow(new_st_enqP));
            end

            // make conflict with others
            wrongSpec_hit_conflict.wset(?);
            wrongSpec_enqIss_conflict.wset(?);
            wrongSpec_enq_conflict.wset(?);
            wrongSpec_update_conflict.wset(?);
            wrongSpec_issue_conflict.wset(?);
            wrongSpec_respLd_conflict.wset(?);
            wrongSpec_deqLd_conflict.wset(?);
            wrongSpec_deqSt_conflict.wset(?);
            wrongSpec_verify_conflict.wset(?);
            wrongSpec_cacheEvict_conflict.wset(?);
            wrongSpec_wakeBySB_conflict.wset(?);
            // more urgent than firstSt
            wrongSpec_urgent_firstSt <= True;
        endmethod
    endinterface
endmodule
