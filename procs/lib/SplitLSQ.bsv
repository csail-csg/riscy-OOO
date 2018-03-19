
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
import CCTypes::*;
import Vector::*;
import GetPut::*;
import Assert::*;
import Ehr::*;
import HasSpecBits::*;
import SpecFifo::*;
import StoreBuffer::*;
import Exec::*;

// I don't want to export auxiliary functions, so manually export all types
export LdQMemFunc(..);
export StQMemFunc(..);
export LdQDeqEntry(..);
export StQDeqEntry(..);
export LSQUpdateAddrResult(..);
export LSQForwardResult(..);
export LSQIssueLdResult(..);
export LSQIssueLdInfo(..);
export LSQKillLdInfo(..);
export LSQRespLdResult(..);
export LSQHitInfo(..);
export SplitLSQ(..);
export mkSplitLSQ;
export isLdQMemFunc;
export isStQMemFunc;

// state transition
// Ld: enq and Idle -> set computed |-> issue and Executing |-> resp and Done |-> Deq
//                                  |                       |                 |-> get killed and set ldKilled
//                                  |                       |-> get killed and set ldKilled
//                                  |-> issue fail and set depXXX -> reset depXXX
// St: enq and Idle -> set computed -> deq
// Lr/Sc/Amo: enq and Idle -> set computed -> issue to mem -> get resp and deq
// Fence: enq and Idle -> deq

// XXX Currently I rely on the fact that specbits of an entry in LSQ contain
// the spectag of itself, so I could kill load easily via specUpdate interface
// when load speculation fails.

// XXX The load specbits in ROB must contain the spectag of itself right after
// address translation, because any exception caused by addr translation should
// kill LSQ entry but not ROB entry, but failed load speculation should kill
// both ROB and LSQ entries

// XXX When detecting eagerly executed loads, we only consider aligned addr for
// dword granularity This reduces the complexity of checking, i.e. we don't
// consider the byte en of each mem access But This forces all accesses to the
// same dword addr to be executed in order

// XXX I choose to kill an eager load L even when it is still executing.  An
// alternative way is to re-execute L. However, this won't work well with
// bypassing load result.  Consider L is forced to re-execute because an older
// load L1 updates the LSQ.  When L got into the cache, it wakes up another
// load L2 which depends on the result of L.  L will wait for L1 to first
// execute. Suppose the store set says that L1 may dependend on a store S.
// Then L2 will stuck at the reg read stage before S resolves its addr.
// However, if S is waken up from reservation station after L2, then we
// deadlock.

// TODO Fence is currently not inserted into LSQ. we need to set its aq/rl
// bits in future and put it into LSQ

// For TSO, to make our OOO execution conform to TSO, we need to *verify* each
// load and store sequentially to mimic TSO I2E operational model. Besides the
// sequential ordering of verification, a Ld can be verified when it has its
// result, a St can be verified when it has addr and data computed, an
// Lr/Sc/Amo can be verified when it has completed memory access, and a fence
// can be verified when all older mem accesses have been dequeued from LSQ. A
// Ld/Lr/MMIO is verified by dequeuing it from LQ, so there is no
// verify-pointer in LQ. A Sc/Amo/Fence/MMIO is also verfied by dequeuing it
// from SQ. The only reason for needing a verify-pointer in SQ is that a
// verified St can still live in SQ. All stores within [deqP, verify-pointer)
// are Sts that have been verfied, and verify-pointer points to the next store
// to be verfied.  This pointer helps enforcing the sequential verification
// ordering:
// - The LQ head can be dequeued when its olderSt is invalid or older than
// SQ verify pointer.
// - The SQ entry at verify pointer can be verified when the LQ head's
// olderSt is younger than or equal to this SQ entry.
// Because of the pathological case that verify-ptr = deq ptr = enq ptr (when
// SQ is full), we need to add a verified bit to SQ to distinguish the case of
// everything verified from the case of nothing verified.

// For WEAK, we also need verified bits and verify-ptr for SQ. These are for
// dequeuing LQ. An LQ entry cannot be dequeud until all older SQ entries have
// computed their addresses (pure fences can be verified as long as they are
// valid). Note that fences do not prevent normal Ld from being dequeued, but
// prevent normal Ld from executing. For Lr, things are trickier. Lr is issued
// at deq time. To avoid doing a search for fences, we can just issue Lr when
// all older SQ entries are dequeued. Same thing applies to MMIO. We add these
// to guards of deqLd. The requirement for verification is slightly different
// from TSO, i.e., an Sc/Amo can be verified by just computing the addr; it
// needs not to be dequeued from SQ; and older LQ entries can still exist.

typedef enum {Ld, Lr} LdQMemFunc deriving(Bits, Eq, FShow);

// LQ holds Ld and Lr. This type is for documentation purpose, it is not really
// used.
typedef struct {
    // ===================
    // Basic info

    InstTag          instTag;
    LdQMemFunc       memFunc;
    Bool             unsignedLd;
    Bool             byteEn; // unshifted BE
    Bool             acq; // acquire ordering
    Bool             rel; // release ordering
    Maybe#(PhyDst)   dst;
    Addr             paddr;
    // whether paddr is mmio addr; MMIO access is handled at deq time
    // non-speculatively
    Bool             isMMIO;
    // byte enable after shift to align with dword boudary. This is valid
    // for all types of memory accesses.
    ByteEn           shiftedBE;

    // ===================
    // Status bits of the Ld. Typically we don't need to reset any bit as the
    // Ld move forward. The only exception is the inIssueQ bit. It is reset
    // when Ld issues from issueQ. This is because it may need to be set again
    // if the Ld finds itself stalled by something.

    // paddr/isMMIO/data have been computed
    Bool             computed;
    // Ld is in issueQ, can be true only when computed = True and excutiong =
    // done = killed = False
    Bool             inIssueQ;
    // Ld is executing (either issued to cache or forwarding is on its way).
    Bool             executing;
    // Ld has got its result (executing must be true)
    Bool             done;
    // Ld is killed by older inst (failed speculation), can be true only when
    // executing or done is true
    Bool             killed;

    // ===================
    // Ld/St ordering.

    // When the Ld allocates the LQ entry, we record the tag of the youngest SQ
    // entry, i.e., the youngest store that is older than the Ld. This is used
    // for searching older stores for bypass or stall. If the SQ entry is not
    // valid, then the search is not needed. 
    Maybe#(StQTag)   olderSt;
    // We can deq a LQ entry when older SQ entries have been dequeued or
    // verified. The olderSt field can indication if the immeidate older SQ
    // entry have been dequeued, this field indicates whether it is
    // verified. This is only meaningful when olderSt is valid.
    Bool             olderStVerified;
    // The store that the Ld reads from. If invalid, then the load reads from
    // memory. This only meaning when done = True.
    Maybe#(StQTag)   readFrom;

    // ===================
    // Reasons for Ld stall, can be non-Invalid only when inIssueQ = executing
    // = done = killed = False. Though only 1 reason can be valid, we keep them
    // separately to avoid bypassing when these reasons are reset in various
    // events.

    // Ld stalled by Lr to same addr, should wait for it to deq
    Maybe#(LdQTag)   depLdQDeq;
    // Ld stalled by St/Sc/Amo in SQ to same addr, wait for it to deq
    Maybe#(StQTag)   depStQDeq;
`ifndef TSO_MM
    // WEAK model only: Ld stalled by unexecuted Ld to same addr, should wait
    // it to be issued
    Maybe#(LdQTag)   depLdEx;
    // WEAK model only: Ld stalled by store buffer entry, should wait it to
    // write cache
    Maybe#(SBIndex)  depSBDeq;
`endif

    // ===================
    // Speculation related.

    // after update with paddr, all Lds and all MMIOs should have valid spec
    // tag
    Maybe#(SpecTag)  specTag;
    // spec bits should contain myself's spec tag
    SpecBits         specBits;
    // waiting for a wrong path load resp (the current entry cannot issue)
    Bool             waitWPResp;
} LdQEntry deriving (Bits, Eq, FShow);

typedef enum {St, Sc, Amo} StQMemFunc deriving(Bits, Eq, FShow);

// SQ holds St, Sc and Amo. This type is for documentation purpose, it is not
// really used.
typedef struct {
    // ===================
    // Basic info

    InstTag          instTag;
    StQMemFunc       memFunc;
    AmoFunc          amoFunc;
    Bool             byteEn; // unshifted BE
    Bool             acq; // acquire ordering
    Bool             rel; // release ordering
    Maybe#(PhyDst)   dst;
    // keep full addr, because  we use addr[2] to determine upper or lower
    // 32-bit AMO in cache, and we use lower bits of addr to shift load resp.
    Addr             paddr;
    // whether paddr is mmio addr; MMIO access is handled at deq time
    // non-speculatively
    Bool             isMMIO;
    // byte enable after shift to align with dword boudary. This is valid
    // for all types of memory accesses.
    ByteEn           shiftedBE;
    // St/Sc/Amo data
    // for St/Sc: store data after shift to align with dword boudary
    // for Amo: data is **NOT** shifted, this doesn't affect forwarding to Ld,
    // because AMO never forwards data
    Data             stData;

    // ===================
    // status bits of St/Sc/Amo

    // paddr/isMMIO/data have been computed
    Bool             computed;
    // entry is verified, see comments above
    Bool             verified;

    // ===================
    // Ld/St ordering.

    // Unfortunately, we choose not to record the younger Ld index at enq time
    // (doing so can save 1 virtual tag computation on the whole LQ). This is
    // because the LQ may be full when the SQ is enq, and we don't know what to
    // record.

    // ===================
    // Speculation related.

    // after update with paddr, all all MMIOs should have valid spec tag
    Maybe#(SpecTag)  specTag;
    // spec bits should contain myself's spec tag
    SpecBits         specBits;
} StQEntry deriving(Bits, Eq, FShow);

typedef struct {
    Bool waitWPResp;
} LSQUpdateAddrResult deriving(Bits, Eq, FShow);

typedef struct {
    Maybe#(PhyDst) dst;
    Data data; // align with dword, not final result written to reg file
} LSQForwardResult deriving(Bits, Eq, FShow);

typedef union tagged {
    void ToCache;
    void Stall;
    LSQForwardResult Forward;
} LSQIssueLdResult deriving(Bits, Eq, FShow);

typedef struct {
    LdQTag tag;
    Addr paddr;
    ByteEn shiftedBE;
} LSQIssueLdInfo deriving(Bits, Eq, FShow);

typedef struct {
    InstTag instTag;
    Maybe#(SpecTag) specTag;
} LSQKillLdInfo deriving(Bits, Eq, FShow);

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
    InstTag         instTag;
    LdQMemFunc      memFunc;
    ByteEn          byteEn;
    Bool            unsignedLd;
    Bool            rel;
    Maybe#(PhyDst)  dst;
    Addr            paddr;
    Bool            isMMIO;
    ByteEn          shiftedBE;
    Maybe#(SpecTag) specTag;
    SpecBits        specBits;
    Bool            waitWPResp; // TODO actually not needed
} LdQDeqEntry deriving (Bits, Eq, FShow);

typedef struct {
    InstTag         instTag;
    StQMemFunc      memFunc;
    AmoFunc         amoFunc;
    Bool            acq;
    Bool            rel;
    Maybe#(PhyDst)  dst;
    Addr            paddr;
    Bool            isMMIO;
    ByteEn          shiftedBE;
    Data            stData;
    Maybe#(SpecTag) specTag;
    SpecBits        specBits;
} StQDeqEntry deriving (Bits, Eq, FShow);

interface SplitLSQ;
    // Enq at renaming. We split to 2 enq methods to enable synthesize
    // boundary. If we merge into 1 enq method, the guard will depend on the
    // type of mem inst, so cannot be synthesized.
    method Maybe#(LdStQTag) enqLdTag;
    method Maybe#(LdStQTag) enqStTag;
    method Action enqLd(InstTag inst_tag,
                        MemInst mem_inst,
                        Maybe#(PhyDst) dst,
                        SpecBits spec_bits);
    method Action enqSt(InstTag inst_tag,
                        MemInst mem_inst,
                        Maybe#(PhyDst) dst,
                        SpecBits spec_bits);
    // A mem inst needs orignal BE (not shifted) at addr translation
    method ByteEn getOrigBE(LdStQTag t);
    // Retrieve information when we want to wakeup RS early in case
    // Ld/Lr/Sc/Amo hits in cache
    method ActionValue#(LSQHitInfo) getHit(LdStQTag t);
    // update store data (shifted for St and Sc, unshifted for AMO). XXX we
    // assume data is updated before addr is updated
    method Action updateData(StQTag t, Data d);
    // Update addr after address translation, and set the spec tag if it is a
    // load or MMIO. Also search for the (oldest) younger load to kill. Return
    // if the entry is waiting for wrong path resp.
    method ActionValue#(LSQUpdateAddrResult) updateAddr(
        LdStQTag lsqTag, Addr paddr, Bool isMMIO,
        ByteEn shiftedBE, Maybe#(SpecTag) specTag
    );
    // Issue a load, and remove dependence on this load issue.
    method ActionValue#(LSQIssueLdResult) issueLd(
        LdQTag lsqTag, Addr paddr, ByteEn shiftedBE, SBSearchRes sbRes
    );
    // Get the load to issue
    method ActionValue#(LSQIssueLdInfo) getIssueLd;
    // Get the load killed by ld/st ordering
    method ActionValue#(LSQKillLdInfo) getLdKilledByLdSt;
    // Get load resp
    method ActionValue#(LSQRespLdResult) respLd(LdQTag t, Data alignedData);
    // Deq LQ entry, and wakeup stalled loads. The guard checks the following:
    // (1) valid
    // (2) not killed (we cannot deq a killed Ld and then commit it in ROB)
    // (3) for non-MMIO Ld, it is done and all older SQ entries have been
    // dequeued or verified
    // (4) for Lr or MMIO, it is computed and there is no older SQ entry (this
    // also handles the .rl associated with Lr)
    // NOTE: there is no pure fence in LQ right now
    // Outside world should do the following:
    // (1) issue Lr or MMIO to memory system only at deq port
    // (2) check specBits (and waitWPResp TODO maybe not needed) before issuing
    // Lr or MMIO
    // (3) For WEAK model, check .rl associated with Lr and SB empty before
    // issuing Lr
    // (4) set ROB entry of deq mem inst to Executed (so that ROB can commit)
    // (5) when deq Ld or MMIO, clear spectag globally
    // (6) Fore WEAK model, before issuing (non-MMIO) Lr, ensure SB does not
    // contain overlapping address
    method LdQDeqEntry firstLd;
    method Action deqLd;
    // Deq SQ entry, and wakeup stalled loads. Also change the readFrom and
    // olderSt fields of loads. The guard only checks the following:
    // (1) valid
    // (2) for St/Sc/Amo/MMIO, it is computed and all older LQ entries have
    // been dequeued
    // NOTE: .rl associated with Sc/Amo is automatically handled, and there is
    // no pure fence in SQ right now.
    // Outside world should do the following:
    // (1) issue Sc/Amo/MMIO to memory system at deq port
    // (2) for WEAK model, issue normal St to SB
    // (3) for TSO, issue normal St to memory
    // (4) check specBits before issuing to memory or SB
    // (5) For WEAK model, check .rl associated with Sc/Amo and SB empty before
    // issuing Sc/Amo
    // (6) set ROB entry of dequeued Sc/Amo/MMIO to Executed (normal St should
    // have been set as Executed when addr and data are computed)
    // (7) when deq MMIO, clear spectag globally
    // (8) Fore WEAK model, before issuing non-MMIO Sc/Amo, ensure SB does not
    // contain overlapping address
    method StQDeqEntry firstSt;
    method Action deqSt;
`ifdef TSO_MM
    // Kill loads when a cache line is evicted (TSO only)
    method Action cacheEvict(LineAddr a);
    // Get the load killed by cache eviction (TSO only)
    method ActionValue#(LSQKillLdInfo) getLdKilledByCache;
`else
    // Wake up younger loads when SB deq (only WEAK model has SB)
    method Action wakeupLdStalledBySB(SBIndex sbIdx);
`endif
    // Speculation
    interface SpeculationUpdate specUpdate;
endinterface

// --- auxiliary types and functions ---
// virtual index: 0 -- (2 * size - 1)
typedef Bit#(TLog#(TMul#(2, LdQSize))) LdQVirTag;
typedef Bit#(TLog#(TMul#(2, StQSize))) StQVirTag;

typedef Bit#(TSub#(AddrSz, TLog#(NumBytes))) DataAlignedAddr;
function DataAlignedAddr getDataAlignedAddr(Addr a) = truncateLSB(a);

// whether two memory accesses are to the same dword
function Bool sameAlignedAddr(Addr a, Addr b);
    return getDataAlignedAddr(a) == getDataAlignedAddr(b);
endfunction

// whether two memory accesses overlap
function Bool overlapAddr(Addr addr_1, ByteEn shift_be_1,
                          Addr addr_2, ByteEn shift_be_2);
    Bool be_overlap = (pack(shift_be_1) & pack(shift_be_2)) != 0;
    return be_overlap && sameAlignedAddr(addr_1, addr_2);
endfunction

// check shiftBE1 covers shiftBE2
function Bool be1CoverBe2(ByteEn shift_be_1, ByteEn shift_be_2);
    return (pack(shift_be_1) & pack(shift_be_2)) == pack(shift_be_2);
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

// issueQ of LSQ tags for issue
typedef SpecFifo_SB_deq_enq_C_deq_enq#(2, LSQIssueLdInfo) LSQIssueLdQ;
(* synthesize *)
module mkLSQIssueLdQ(LSQIssueLdQ);
    let m <- mkSpecFifo_SB_deq_enq_C_deq_enq(True);
    return m;
endmodule

// killQ of LSQ tags should be killed
typedef SpecFifo_SB_deq_enq_SB_deq_wrong_C_enq#(2, LdQTag) LSQKillLdQ;
(* synthesize *)
module mkLSQKillLdQ(LSQKillLdQ);
    let m <- mkSpecFifo_SB_deq_enq_SB_deq_wrong_C_enq(True);
    return m;
endmodule
// --- end of auxiliary types and functions ---

(* synthesize *)
module mkSplitLSQ(SplitLSQ);
    // method/rule ordering
    // getHit, getKillLd, findIssue <
    // (deqLd (TSO ? C : <) verifySt) <
    // cacheEvict <
    // updateAddr <
    // issueLd, getIssueLd <
    // enqIssueQ <
    // (wakeupLdStalledBySB (Weak only) CF deqSt) <
    // respLd <
    // updateData <
    // (enqLd C enqSt) <
    // correctSpec

    // Scheduling notes:
    // - getKilled, getHit, findIssue are almost readonly, so put them at
    // beginning.
    // - findIssue must be before updateAddr, because the newly updated load
    // may be issued outside LSQ. We don't want to enq this Ld to issueQ.
    // - A load can first updateAddr and then issue in one cycle (in two
    // rules), so updateAddr < issueLd. Also, issueLd writes readFrom which is
    // used in the associative search in updateAddr.
    // - cacheEvict and updateAddr needs the readFrom fields in LQ. Since deqSt
    // changes readFrom, we put cacheEvict, updateAddr < deqSt.
    // - issueLd needs the olderSt field and SQ. Since deqSt changeds SQ and
    // readFrom and olderSt fields of LQ, we put issueLd < deqSt. However,
    // since issueLd sets readFrom and depStQDeq, this creates a bypassing path
    // from issueLd to deqSt.
    // - In WEAK model, issueLd will search store buffer. Since
    // wakeupLdStalledBySB happens in the same rule as store buffer deq, we put
    // wakeupLdStalledBySB > issueLd. However, since issueLd sets depSBDeq,
    // this creates a bypassing path from issueLd to wakeupLdStalledBySB. This
    // is pretty much aligned with the case of deqSt.
    // - There should not be requirement between cachEvict and updateAddr,
    // just choose arbitrarily.
    // - Since load resp may be enq into a bypass fifo after coming out of the
    // cache, respLd should not precede the cache rule that sends resp. We just
    // put the cache resp rule < respLd. Since cache resp rule calls deqSt
    // (TSO) or wakeupLdStalledBySB (WEAK), we have deqSt < respLd and
    // wakeupLdStalledBySB < respLd.
    // - Since respLd needs shiftedBE, we put respLd < updateData.
    // - There is a bypassing path from updateAddr to deqSt. This path should
    // not be activated in reality, because the updated StQ entry cannot be
    // validated in the same cycle. TODO? If we really want, we can make a
    // bypass wire to read paddr[0] in deqSt.
    // - To cut off bypassing from stb.enq to stb.deq, we put wakeupStallBySB
    // < deqSt.
    // - There is a bypassing path from updateAddr to respLd, though it should
    // never be activated in reality. TODO We can make a bypass wire to read
    // paddr[0] in respLd method.
    // - There is a bypassing path from validatedSt to deqSt, i.e., a store
    // can be validated and issued to memory at the same cycle. This is
    // probably a desirable behavior.
    // - We put findIssue < updateAddr to prevent findIssue insert a newly
    // updated LQ entry into issueQ; this can create problem because the new
    // updated load may be immediately issued to execution in the same cycle
    // later.
    // - In TSO, only one of deqLd and verifySt can fire at one cycle, because
    // of sequential verification requirement. In WEAK, deqLd < verifySt,
    // because verifySt does not need to peek any info in LQ.

    // W.r.t wrongSpec:
    // getKillLd < wrongSpec (they fire in same rule)
    // findIss, getHit < wrongSpec (findIss ad getHit are read only)
    // All other methods or rules that have conflicting accesses with wrongSpec
    // should conflict with wrongSpec to cut off any possible bypass path.

    // XXX Since firstSt is ordered very late, we are likely to end up with
    // wrongSpec < firstSt. Thus, if we call wrongSpec and firstSt together in
    // one rule, then we may end up with a cycle in scheduling (with another
    // rule that calls firstLd and deqLd in case wrongSpec < deqLd). Therefore,
    // when we need to call wrongSpec from firstSt (typically when an MMIO
    // request faults), we should first copy the MMIO request to a reg, and
    // then kill using the info in reg.

    // LQ
    // entry valid bits
    Vector#(LdQSize, Ehr#(2, Bool))             ld_valid           <- replicateM(mkEhr(False));
    // entry contents
    Vector#(LdQSize, Reg#(InstTag))             ld_instTag         <- replicateM(mkRegU);
    Vector#(LdQSize, Reg#(LdQMemFunc))          ld_memFunc         <- replicateM(mkRegU);
    Vector#(LdQSize, Reg#(Bool))                ld_unsigned        <- replicateM(mkRegU);
    Vector#(LdQSize, Reg#(ByteEn))              ld_byteEn          <- replicateM(mkRegU);
    Vector#(LdQSize, Reg#(Bool))                ld_acq             <- replicateM(mkRegU);
    Vector#(LdQSize, Reg#(Bool))                ld_rel             <- replicateM(mkRegU);
    Vector#(LdQSize, Reg#(Maybe#(PhyDst)))      ld_dst             <- replicateM(mkRegU);
    Vector#(LdQSize, Ehr#(2, Addr))             ld_paddr           <- replicateM(mkEhr(?));
    Vector#(LdQSize, Ehr#(2, Bool))             ld_isMMIO          <- replicateM(mkEhr(?));
    Vector#(LdQSize, Ehr#(2, ByteEn))           ld_shiftedBE       <- replicateM(mkEhr(?));
    Vector#(LdQSize, Ehr#(2, Bool))             ld_computed        <- replicateM(mkEhr(?));
    Vector#(LdQSize, Ehr#(3, Bool))             ld_inIssueQ        <- replicateM(mkEhr(?));
    Vector#(LdQSize, Ehr#(2, Bool))             ld_executing       <- replicateM(mkEhr(?));
    Vector#(LdQSize, Ehr#(2, Bool))             ld_done            <- replicateM(mkEhr(?));
    Vector#(LdQSize, Ehr#(3, Bool))             ld_killed          <- replicateM(mkEhr(?));
    Vector#(LdQSize, Ehr#(2, Maybe#(StQTag)))   ld_olderSt         <- replicateM(mkEhr(?));
    Vector#(LdQSize, Ehr#(2, Bool))             ld_olderStVerified <- replicateM(mkEhr(?));
    Vector#(LdQSize, Ehr#(3, Maybe#(StQTag)))   ld_readFrom        <- replicateM(mkEhr(?));
    Vector#(LdQSize, Ehr#(3, Maybe#(LdQTag)))   ld_depLdQDeq       <- replicateM(mkEhr(?));
    Vector#(LdQSize, Ehr#(3, Maybe#(StQTag)))   ld_depStQDeq       <- replicateM(mkEhr(?));
`ifndef TSO_MM
    Vector#(LdQSize, Ehr#(3, Maybe#(LdQTag)))   ld_depLdEx         <- replicateM(mkEhr(?));
    Vector#(LdQSize, Ehr#(3, Maybe#(SBIndex)))  ld_depSBDeq        <- replicateM(mkEhr(?));
`endif
    Vector#(LdQSize, Ehr#(3, Maybe#(SpecTag)))  ld_specTag         <- replicateM(mkEhr(?));
    Vector#(LdQSize, Ehr#(3, SpecBits))         ld_specBits        <- replicateM(mkEhr(?));
    // wrong-path load filter (must init to all False)
    Vector#(LdQSize, Ehr#(1, Bool))             ld_waitWPResp      <- replicateM(mkEhr(False));
    // enq/deq ptr
    Reg#(LdQTag)    ld_enqP <- mkReg(0);
    Ehr#(2, LdQTag) ld_deqP <- mkEhr(0);

    // Ports of each EHR used in each method or rule ("write" means the write
    // port is used, "assert" means the port is only used for assert in
    // simulation)
    let ld_valid_getKill   = getVEhrPort(ld_valid, 0);
    let ld_valid_findIss   = getVEhrPort(ld_valid, 0);
    let ld_valid_wrongSpec = getVEhrPort(ld_valid, 0); // write
    let ld_valid_deqLd     = getVEhrPort(ld_valid, 0); // write
    let ld_valid_verify    = getVEhrPort(ld_valid, 0); // only for TSO, C with deqLd
    let ld_valid_evict     = getVEhrPort(ld_valid, 1);
    let ld_valid_updAddr   = getVEhrPort(ld_valid, 1);
    let ld_valid_issue     = getVEhrPort(ld_valid, 1);
    let ld_valid_enqIss    = getVEhrPort(ld_valid, 1); // assert
    let ld_valid_deqSt     = getVEhrPort(ld_valid, 1);
    let ld_valid_resp      = getVEhrPort(ld_valid, 1); // assert
    let ld_valid_enq       = getVEhrPort(ld_valid, 1); // write

    let ld_paddr_findIss = getVEhrPort(ld_paddr, 0);
    let ld_paddr_deqLd   = getVEhrPort(ld_paddr, 0);
    let ld_paddr_evict   = getVEhrPort(ld_paddr, 0);
    let ld_paddr_updAddr = getVEhrPort(ld_paddr, 0); // write
    let ld_paddr_issue   = getVEhrPort(ld_paddr, 1);
    let ld_paddr_enqIss  = getVEhrPort(ld_paddr, 1); // assert
    let ld_paddr_resp    = getVEhrPort(ld_paddr, 1);

    let ld_isMMIO_findIss = getVEhrPort(ld_isMMIO, 0);
    let ld_isMMIO_evict   = getVEhrPort(ld_isMMIO, 0); // assert
    let ld_isMMIO_deqLd   = getVEhrPort(ld_isMMIO, 0);
    let ld_isMMIO_updAddr = getVEhrPort(ld_isMMIO, 0); // write
    let ld_isMMIO_issue   = getVEhrPort(ld_isMMIO, 1); // assert
    let ld_isMMIO_enqIss  = getVEhrPort(ld_isMMIO, 1); // assert

    let ld_shiftedBE_findIss = getVEhrPort(ld_shiftedBE, 0);
    let ld_shiftedBE_deqLd   = getVEhrPort(ld_shiftedBE, 0);
    let ld_shiftedBE_updAddr = getVEhrPort(ld_shiftedBE, 0); // write
    let ld_shiftedBE_issue   = getVEhrPort(ld_shiftedBE, 1);
    let ld_shiftedBE_enqIss  = getVEhrPort(ld_shiftedBE, 1); // assert

    let ld_computed_findIss = getVEhrPort(ld_computed, 0);
    let ld_computed_getKill = getVEhrPort(ld_computed, 0); // assert
    let ld_computed_deqLd   = getVEhrPort(ld_computed, 0);
    let ld_computed_evict   = getVEhrPort(ld_computed, 0); // assert
    let ld_computed_updAddr = getVEhrPort(ld_computed, 0); // write
    let ld_computed_issue   = getVEhrPort(ld_computed, 1);
    let ld_computed_enqIss  = getVEhrPort(ld_computed, 1); // assert
    let ld_computed_resp    = getVEhrPort(ld_computed, 1); // assert
    let ld_computed_enq     = getVEhrPort(ld_computed, 1); // write

    let ld_inIssueQ_findIss = getVEhrPort(ld_inIssueQ, 0);
    let ld_inIssueQ_updAddr = getVEhrPort(ld_inIssueQ, 0); // assert
    let ld_inIssueQ_issue   = getVEhrPort(ld_inIssueQ, 0); // write
    let ld_inIssueQ_enqIss  = getVEhrPort(ld_inIssueQ, 1); // write
    let ld_inIssueQ_enq     = getVEhrPort(ld_inIssueQ, 2); // write

    let ld_executing_findIss   = getVEhrPort(ld_executing, 0);
    let ld_executing_wrongSpec = getVEhrPort(ld_executing, 0);
    let ld_executing_getKill   = getVEhrPort(ld_executing, 0); // assert
    let ld_executing_evict     = getVEhrPort(ld_executing, 0);
    let ld_executing_updAddr   = getVEhrPort(ld_executing, 0);
    let ld_executing_issue     = getVEhrPort(ld_executing, 0); // write
    let ld_executing_enqIss    = getVEhrPort(ld_executing, 1); // assert
    let ld_executing_resp      = getVEhrPort(ld_executing, 1); // assert
    let ld_executing_enq       = getVEhrPort(ld_executing, 1); // write

    let ld_done_wrongSpec = getVEhrPort(ld_done, 0);
    let ld_done_deqLd     = getVEhrPort(ld_done, 0);
    let ld_done_updAddr   = getVEhrPort(ld_done, 0); // assert
    let ld_done_issue     = getVEhrPort(ld_done, 0); // assert
    let ld_done_enqIss    = getVEhrPort(ld_done, 0); // assert
    let ld_done_resp      = getVEhrPort(ld_done, 0); // write
    let ld_done_enq       = getVEhrPort(ld_done, 1); // write

    let ld_killed_getKill = getVEhrPort(ld_killed, 0); // assert
    let ld_killed_deqLd   = getVEhrPort(ld_killed, 0);
    let ld_killed_evict   = getVEhrPort(ld_killed, 0); // write
    let ld_killed_updAddr = getVEhrPort(ld_killed, 1); // write
    let ld_killed_issue   = getVEhrPort(ld_killed, 2); // assert
    let ld_killed_enqIss  = getVEhrPort(ld_killed, 2); // assert
    let ld_killed_enq     = getVEhrPort(ld_killed, 2); // write

    let ld_olderSt_deqLd  = getVEhrPort(ld_olderSt, 0);
    let ld_olderSt_verify = getVEhrPort(ld_olderSt, 0);
    let ld_olderSt_deqSt  = getVEhrPort(ld_olderSt, 0); // write
    let ld_olderSt_enq    = getVEhrPort(ld_olderSt, 1); // write

    let ld_olderStVerified_deqLd  = getVEhrPort(ld_olderStVerified, 0);
    let ld_olderStVerified_verify = getVEhrPort(ld_olderStVerified, 0); // write
    let ld_olderStVerified_enq    = getVEhrPort(ld_olderStVerified, 1); // write

    let ld_readFrom_evict = getVEhrPort(ld_readFrom, 0);
    let ld_readFrom_issue = getVEhrPort(ld_readFrom, 0); // write
    let ld_readFrom_deqSt = getVEhrPort(ld_readFrom, 1); // write
    let ld_readFrom_enq   = getVEhrPort(ld_readFrom, 2); // write

    let ld_depLdQDeq_findIss = getVEhrPort(ld_depLdQDeq, 0);
    let ld_depLdQDeq_deqLd   = getVEhrPort(ld_depLdQDeq, 0); // write
    let ld_depLdQDeq_issue   = getVEhrPort(ld_depLdQDeq, 1); // write
    let ld_depLdQDeq_enqIss  = getVEhrPort(ld_depLdQDeq, 2); // assert
    let ld_depLdQDeq_enq     = getVEhrPort(ld_depLdQDeq, 2); // write

    let ld_depStQDeq_findIss = getVEhrPort(ld_depStQDeq, 0);
    let ld_depStQDeq_issue   = getVEhrPort(ld_depStQDeq, 0); // write
    let ld_depStQDeq_enqIss  = getVEhrPort(ld_depStQDeq, 1); // assert
    let ld_depStQDeq_deqSt   = getVEhrPort(ld_depStQDeq, 1); // write
    let ld_depStQDeq_enq     = getVEhrPort(ld_depStQDeq, 2); // write

`ifndef TSO_MM
    let ld_depLdEx_findIss = getVEhrPort(ld_depLdEx, 0);
    let ld_depLdEx_issue   = getVEhrPort(ld_depLdEx, 0); // write
    let ld_depLdEx_enqIss  = getVEhrPort(ld_depLdEx, 1); // assert
    let ld_depLdEx_enq     = getVEhrPort(ld_depLdEx, 1); // write

    let ld_depSBDeq_findIss = getVEhrPort(ld_depSBDeq, 0);
    let ld_depSBDeq_issue   = getVEhrPort(ld_depSBDeq, 0); // write
    let ld_depSBDeq_enqIss  = getVEhrPort(ld_depSBDeq, 1); // assert
    let ld_depSBDeq_wakeSB  = getVEhrPort(ld_depSBDeq, 1); // write
    let ld_depSBDeq_enq     = getVEhrPort(ld_depSBDeq, 2); // write
`endif

    let ld_specTag_getKill = getVEhrPort(ld_specTag, 0);
    let ld_specTag_deqLd   = getVEhrPort(ld_specTag, 0);
    let ld_specTag_evict   = getVEhrPort(ld_specTag, 0); // assert
    let ld_specTag_updAddr = getVEhrPort(ld_specTag, 0); // write
    let ld_specTag_enq     = getVEhrPort(ld_specTag, 1); // write

    let ld_specBits_wrongSpec   = getVEhrPort(ld_specBits, 0); // write
    let ld_specBits_deqLd       = getVEhrPort(ld_specBits, 0); // C with wrongSpec
    let ld_specBits_evict       = getVEhrPort(ld_specBits, 0); // C with wrongSpec
    let ld_specBits_updAddr     = getVEhrPort(ld_specBits, 0); // C with wrongSpec
    let ld_specBits_enqIss      = getVEhrPort(ld_specBits, 0); // C with wrongSpec
    let ld_specBits_enq         = getVEhrPort(ld_specBits, 0); // write, C with wrongSpec
    let ld_specBits_correctSpec = getVEhrPort(ld_specBits, 1); // write

    let ld_waitWPResp_hit       = getVEhrPort(ld_waitWPResp, 0);
    let ld_waitWPResp_findIss   = getVEhrPort(ld_waitWPResp, 0);
    let ld_waitWPResp_deqLd     = getVEhrPort(ld_waitWPResp, 0);
    let ld_waitWPResp_updAddr   = getVEhrPort(ld_waitWPResp, 0);
    let ld_waitWPResp_issue     = getVEhrPort(ld_waitWPResp, 0); // assert
    let ld_waitWPResp_enqIss    = getVEhrPort(ld_waitWPResp, 0);
    let ld_waitWPResp_resp      = getVEhrPort(ld_waitWPResp, 0); // write
    let ld_waitWPResp_wrongSpec = getVEhrPort(ld_waitWPResp, 0); // write

    Reg#(LdQTag) ld_deqP_deqLd  = ld_deqP[0]; // write
    Reg#(LdQTag) ld_deqP_verify = ld_deqP[0]; // in TSO, C with deqLd
    Reg#(LdQTag) ld_deqP_deqSt  = ld_deqP[1];

    // SQ
    // entry valid bits
    Vector#(StQSize, Ehr#(2, Bool))            st_valid     <- replicateM(mkEhr(False));
    // entry contents
    Vector#(StQSize, Reg#(InstTag))            st_instTag   <- replicateM(mkRegU);
    Vector#(StQSize, Reg#(StQMemFunc))         st_memFunc   <- replicateM(mkRegU);
    Vector#(StQSize, Reg#(AmoFunc))            st_amoFunc   <- replicateM(mkRegU);
    Vector#(StQSize, Reg#(ByteEn))             st_byteEn    <- replicateM(mkRegU);
    Vector#(StQSize, Reg#(Bool))               st_acq       <- replicateM(mkRegU);
    Vector#(StQSize, Reg#(Bool))               st_rel       <- replicateM(mkRegU);
    Vector#(StQSize, Reg#(Maybe#(PhyDst)))     st_dst       <- replicateM(mkRegU);
    Vector#(StQSize, Ehr#(2, Addr))            st_paddr     <- replicateM(mkEhr(?));
    Vector#(StQSize, Ehr#(2, Bool))            st_isMMIO    <- replicateM(mkEhr(?));
    Vector#(StQSize, Ehr#(2, ByteEn))          st_shiftedBE <- replicateM(mkEhr(?));
    Vector#(StQSize, Ehr#(1, Data))            st_stData    <- replicateM(mkEhr(?));
    Vector#(StQSize, Ehr#(2, Bool))            st_computed  <- replicateM(mkEhr(?));
    Vector#(StQSize, Ehr#(2, Bool))            st_verified  <- replicateM(mkEhr(?));
    Vector#(StQSize, Ehr#(2, Maybe#(SpecTag))) st_specTag   <- replicateM(mkEhr(?));
    Vector#(StQSize, Ehr#(2, SpecBits))        st_specBits  <- replicateM(mkEhr(?));
    // enq/deq ptr
    Reg#(StQTag) st_enqP <- mkReg(0);
    Reg#(StQTag) st_deqP <- mkReg(0);
    Reg#(StQTag) st_verifyP <- mkReg(0);

    let st_valid_wrongSpec = getVEhrPort(st_valid, 0); // write
    let st_valid_verify    = getVEhrPort(st_valid, 0);
    let st_valid_updAddr   = getVEhrPort(st_valid, 0); // assert
    let st_valid_issue     = getVEhrPort(st_valid, 0);
    let st_valid_deqSt     = getVEhrPort(st_valid, 0); // write
    let st_valid_updData   = getVEhrPort(st_valid, 1); // assert
    let st_valid_enq       = getVEhrPort(st_valid, 1); // write

    let st_paddr_updAddr = getVEhrPort(st_paddr, 0); // write
    let st_paddr_issue   = getVEhrPort(st_paddr, 1);
    let st_paddr_deqSt   = getVEhrPort(st_paddr, 1);

    let st_isMMIO_verify  = getVEhrPort(st_isMMIO, 0);
    let st_isMMIO_updAddr = getVEhrPort(st_isMMIO, 0); // write
    let st_isMMIO_deqSt   = getVEhrPort(st_isMMIO, 1);

    let st_shiftedBE_updAddr = getVEhrPort(st_shiftedBE, 0); // write
    let st_shiftedBE_issue   = getVEhrPort(st_shiftedBE, 1);
    let st_shiftedBE_deqSt   = getVEhrPort(st_shiftedBE, 1);
    
    let st_stData_issue   = getVEhrPort(st_stData, 0);
    let st_stData_deqSt   = getVEhrPort(st_stData, 0);
    let st_stData_updData = getVEhrPort(st_stData, 0); // write

    let st_computed_verify  = getVEhrPort(st_computed, 0);
    let st_computed_updAddr = getVEhrPort(st_computed, 0); // write
    let st_computed_issue   = getVEhrPort(st_computed, 1);
    let st_computed_deqSt   = getVEhrPort(st_computed, 1);
    let st_computed_updData = getVEhrPort(st_computed, 1); // assert
    let st_computed_enq     = getVEhrPort(st_computed, 1); // write

    let st_verified_wrongSpec = getVEhrPort(st_verified, 0);
    let st_verified_verify    = getVEhrPort(st_verified, 0); // write
    let st_verified_updAddr   = getVEhrPort(st_verified, 1); // assert
    let st_verified_enq       = getVEhrPort(st_verified, 1); // write

    let st_specTag_updAddr = getVEhrPort(st_specTag, 0); // write
    let st_specTag_deqSt   = getVEhrPort(st_specTag, 1);
    let st_specTag_enq     = getVEhrPort(st_specTag, 1); // write

    let st_specBits_wrongSpec   = getVEhrPort(st_specBits, 0); // write
    let st_specBits_updAddr     = getVEhrPort(st_specBits, 0); // C with wrongSpec
    let st_specBits_deqSt       = getVEhrPort(st_specBits, 0); // C with wrongSpec
    let st_specBits_enq         = getVEhrPort(st_specBits, 0); // write, C with wrongSpec
    let st_specBits_correctSpec = getVEhrPort(st_specBits, 1); // write

    Reg#(StQTag) st_verifyP_verify    = st_verifyP;
    Reg#(StQTag) st_verifyP_wrongSpec = st_verifyP;

    // FIFO of LSQ tags that try to issue, there should be no replication in it
    LSQIssueLdQ issueLdQ <- mkLSQIssueLdQ;
    // XXX We split the search for ready to issue entry into two phases. Phase
    // 1: rule findIssue: find a ready-to-issue entry at the beginning of the
    // cycle. Phase 2: rule enqIssueQ: enq the one found in findIssue into
    // issueQ and set ldInIssueQ We do the split because enq to issueQ must be
    // ordered after getIssueLd method which deq issueQ.  This split is fine
    // because at phase 2, the entry found in phase one should not be changed
    // by any other method.  This is because findIssue < update < issue <
    // enqIssueQ, i.e. update and issue will not affect the entry found in
    // findIssue We use a wire to pass phase 1 result to phase 2.  It is fine
    // that phase 2 dose not fire when phase 1 has fired, next cycle phase 1
    // will redo the work.
    RWire#(LSQIssueLdInfo) issueLdInfo <- mkRWire;

    // FIFO of LSQ tags that should be killed. Replicated tags may exist, but
    // replications will all be killed when the first is processed due to spec
    // bits.
    LSQKillLdQ killByLdStQ <- mkLSQKillLdQ;
`ifdef TSO_MM
    LSQKillLdQ killByCacheQ <- mkLSQKillLdQ;
`endif

    // make wrongSpec conflict with all others (but not correctSpec method and
    // findIssue)
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

    // find youngest LQ entry that satisfy a constraint (i.e. largest tag)
    function Maybe#(LdQTag) findYoungestLd(Vector#(LdQSize, Bool) pred);
        function LdQTag getYounger(LdQTag a, LdQTag b);
            if(!pred[a]) begin
                return b;
            end
            else if(!pred[b]) begin
                return a;
            end
            else begin
                return ldVirTags[a] < ldVirTags[b] ? b : a;
            end
        endfunction
        Vector#(LdQSize, LdQTag) idxVec = genWith(fromInteger);
        LdQTag tag = fold(getYounger, idxVec);
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

    // find youngest SQ entry that satisfy a constraint (i.e. largest tag)
    function Maybe#(StQTag) findYoungestSt(Vector#(StQSize, Bool) pred);
        function StQTag getYounger(StQTag a, StQTag b);
            if(!pred[a]) begin
                return b;
            end
            else if(!pred[b]) begin
                return a;
            end
            else begin
                return stVirTags[a] < stVirTags[b] ? b : a;
            end
        endfunction
        Vector#(StQSize, StQTag) idxVec = genWith(fromInteger);
        StQTag tag = fold(getYounger, idxVec);
        return pred[tag] ? Valid (tag) : Invalid;
    endfunction

    // virtual tags for olderSt port 0. NOTE: it should NOT be used after
    // method deqSt which modifies olderSt
    function Maybe#(StQVirTag) getOlderStVirTag(LdQTag i);
        if(ld_olderSt[i][0] matches tagged Valid .stTag) begin
            return Valid (getStVirTag(stTag));
        end
        else begin
            return Invalid;
        end
    endfunction
    Vector#(LdQSize, Maybe#(StQVirTag)) olderStVirTags = map(
        getOlderStVirTag, genWith(fromInteger)
    );

    // virtual tags for readFrom port 0. NOTE: it should NOT be used after
    // method issueLd which modifies readFrom.
    function Maybe#(StQVirTag) getReadFromVirTag(LdQTag i);
        if(ld_readFrom[i][0] matches tagged Valid .stTag) begin
            return Valid (getStVirTag(stTag));
        end
        else begin
            return Invalid;
        end
    endfunction
    Vector#(LdQSize, Maybe#(StQVirTag)) readFromVirTags = map(
        getReadFromVirTag, genWith(fromInteger)
    );

    // find load ready for issuing when LSQ is not empty:
    // (1) entry valid of load
    // (2) computed
    // (3) not in issueQ
    // (4) not executing (this implies not done)
    // (5) not depend on any thing
    // (6) waitWPResp is False
    // (7) not MMIO
    // Since this rule does not block any other rule, we can let it fire even
    // when it may do nothing
    rule findIssue;
        // find all can issue loads 
        function Bool canIssue(LdQTag i);
            return (
                ld_valid_findIss[i] && ld_memFunc[i] == Ld && // (1) valid load
                ld_computed_findIss[i] && // (2) computed
                !ld_inIssueQ_findIss[i] && // (3) not in issueQ
                !ld_executing_findIss[i] && // (4) not executing (or done)
                !isValid(ld_depLdQDeq_findIss[i]) &&
`ifndef TSO_MM
                !isValid(ld_depLdEx_findIss[i]) &&
                !isValid(ld_depSBDeq_findIss[i]) &&
`endif
                !isValid(ld_depStQDeq_findIss[i]) && // (5) no dependency
                !ld_waitWPResp_findIss[i] && // (6) not wating wrong path resp
                !ld_isMMIO_findIss[i] // (7) not MMIO
            );
        endfunction
        Vector#(LdQSize, Bool) ableToIssue = map(canIssue,
                                                 genWith(fromInteger));

        // find the oldest load to issue (note that we search for valid entry),
        // and record it in wire
        if(findOldestLd(ableToIssue) matches tagged Valid .tag) begin
            issueLdInfo.wset(LSQIssueLdInfo {
                tag: tag,
                paddr: ld_paddr_findIss[tag],
                shiftedBE: ld_shiftedBE_findIss[tag]
            });
        end
    endrule

    rule enqIssueQ(issueLdInfo.wget matches tagged Valid .info);
        // sanity check
        doAssert(ld_valid_enqIss[info.tag],
                 "enq issueQ entry is valid");
        doAssert(ld_memFunc[info.tag] == Ld,
                 "enq issueQ entry is Ld");
        doAssert(ld_computed_enqIss[info.tag],
                 "enq issueQ entry is computed");
        doAssert(!ld_executing_enqIss[info.tag],
                 "enq issueQ entry cannot be executing");
        doAssert(!ld_done_enqIss[info.tag],
                 "enq issueQ entry cannot be done");
        doAssert(!ld_inIssueQ_enqIss[info.tag],
                 "enq issueQ entry cannot be in issueQ");
        doAssert(!ld_killed_enqIss[info.tag],
                 "enq issueQ entry cannot be killed");
        doAssert(!ld_waitWPResp_enqIss[info.tag],
                 "enq issueQ entry cannot wait for wrong path resp");
        doAssert(!ld_isMMIO_enqIss[info.tag],
                 "enq issueQ entry cannot be MMIO");
        doAssert(!isValid(ld_depLdQDeq_enqIss[info.tag]) &&
`ifndef TSO_MM
                 !isValid(ld_depLdEx_enqIss[info.tag]) &&
                 !isValid(ld_depSBDeq_enqIss[info.tag]) &&
`endif
                 !isValid(ld_depStQDeq_enqIss[info.tag]),
                 "enq issueQ entry cannot have dependency");
        doAssert(info.shiftedBE == ld_shiftedBE_enqIss[info.tag],
                 "BE should match");
        doAssert(info.paddr == ld_paddr_enqIss[info.tag],
                 "paddr should match");
        // enq to issueQ & change state (prevent enq this tag again)
        issueLdQ.enq(ToSpecFifo {
            data: info,
            spec_bits: ld_specBits_enqIss[info.tag]
        });
        ld_inIssueQ_enqIss[info.tag] <= True;
        // make conflict with incorrect spec
        wrongSpec_enqIss_conflict.wset(?);
    endrule

    // Verify SQ entry one by one
    // - TSO verify requires:
    // (1) all older loads are dequeued
    // (2) for normal non-MMIO St, addr and data are computed
    // (3) for Sc/Amo/MMIO, it is dequeued (by completing memory access)
    // WEAK verify only requires that addr is computed
    // NOTE that when SQ is full and all verified, verifyP will point to a
    // valid and verified entry
    rule verifySt(st_valid_verify[st_verifyP_verify] &&
                  !st_verified_verify[st_verifyP_verify]);
        StQTag verP = st_verifyP_verify;

        // check if the entry can be verified. We should not fire this rule if
        // entry cannot be verified, because this may block conflicting
        // rules/methods forever.
`ifdef TSO_MM
        // TSO: need to figure out if older LQ entry exists
        LdQTag ldDeqP = ld_deqP_verify;
        Bool no_older_ld;
        if(ld_valid_verify[ldDeqP]) begin
            if(olderStVirTags[ldDeqP] matches tagged Valid .older) begin
                no_older_ld = older >= stVirTags[verP];
            end
            else begin
                // LQ head has no olderSt, so LQ head is older
                no_older_ld = False;
            end
        end
        else begin
            // LQ empty
            no_older_ld = True;
        end
        when(no_older_ld &&
             st_memFunc[verP] == St &&
             !st_isMMIO_verify[verP] &&
             st_computed_verify[verP], noAction);

`else
        // WEAK: just check computed
        when(st_computed_verify[verP], noAction);
`endif

        // mark as verified and move verify ptr
        st_verified_verify[verP] <= True;
        st_verifyP_verify <= getNextStPtr(verP);

        // tell LQ entries that this entry is verified; no need to check LQ
        // entry valid
        function Action setVerified(LdQTag i);
        action
            if(ld_olderSt_verify[i] == Valid (verP)) begin
                ld_olderStVerified_verify[i] <= True;
            end
        endaction
        endfunction
        Vector#(LdQSize, LdQTag) idxVec = genWith(fromInteger);
        joinActions(map(setVerified, idxVec));

        // make conflict with incorrect spec
        wrongSpec_verify_conflict.wset(?);
    endrule

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

    (* fire_when_enabled, no_implicit_conditions *)
    rule checkStQVerified;
        let valid_verified = zipWith(\&& ,
                                     readVEhr(0, st_valid),
                                     readVEhr(0, st_verified));
        if(all(\== (False), valid_verified)) begin
            // nothing is valid and verified
            doAssert(st_verifyP == st_deqP,
                     "nothing verified, so verifyP = deqP");
        end
        else begin
            // SQ is not empty, and some valid entry is verified, verified
            // entries should be in [deqP, verifyP)
            function Bool in_range(StQTag i);
                if(st_deqP < st_verifyP) begin
                    return st_deqP <= i && i < st_verifyP;
                end
                else begin
                    return st_deqP <= i || i < st_verifyP;
                end
            endfunction
            for(Integer i = 0; i < valueof(StQSize); i = i+1) begin
                if(st_valid[i][0]) begin
                    doAssert(in_range(fromInteger(i)) == st_verified[i][0],
                             "verified entries must be within [deqP, verifyP)");
                end
            end
        end
    endrule

    (* fire_when_enabled, no_implicit_conditions *)
    rule checkLdQVerified;
        for(Integer i = 0; i < valueof(LdQSize); i = i+1) begin
            if (ld_valid[i][0] &&&
                ld_olderSt[i][0] matches tagged Valid .stTag) begin
                doAssert(st_valid[stTag][0], "older SQ entry must be valid");
                doAssert(ld_olderStVerified[i][0] == st_verified[stTag][0],
                         "LdQ olderStVerified does not match StQ verified");
            end
        end
    endrule
`endif

    // lazy enq guard signal
    Wire#(Bool) ld_can_enq_wire <- mkBypassWire;
    Wire#(Bool) st_can_enq_wire <- mkBypassWire;
    (* fire_when_enabled, no_implicit_conditions *)
    rule setForEnq;
        ld_can_enq_wire <= !ld_valid[st_enqP][0];
        st_can_enq_wire <= !st_valid[st_enqP][0];
    endrule

    // deqLd guard (see comments at the method declaration)
    function Bool deqLdGuard;
        LdQTag deqP = ld_deqP_deqLd;
        Bool valid_not_killed = ld_valid_deqLd[deqP] && !ld_killed_deqLd[deqP];
        // Now figure out access type specific requirement
        Bool no_older_st = !isValid(ld_olderSt_deqLd[deqP]);
        if(ld_memFunc[deqP] == Ld && !ld_isMMIO_deqLd[deqP]) begin
            // normal non-MMIO Ld (cannot have .rl)
            return valid_not_killed && ld_done_deqLd[deqP] &&
                   (no_older_st || ld_olderStVerified_deqLd[deqP]);
        end
        else begin
            // Lr or MMIO
            return valid_not_killed && ld_computed_deqLd[deqP] && no_older_st;
        end
    endfunction

    // deqSt guard (see comments at the method declaration)
    function Bool deqStGuard;
        StQTag deqP = st_deqP;
        Bool valid = st_valid_deqSt[deqP];
        Bool computed = st_computed_deqSt[deqP];
        LdQTag ldDeqP = ld_deqP_deqSt;
        Bool no_older_ld = !ld_valid_deqSt[ldDeqP] ||
                           isValid(ld_olderSt_deqSt[ldDeqP]);
        return valid && computed && no_older_ld;
    endfunction

    method ByteEn getOrigBE(LdStQTag t);
        return (case(t) matches
            tagged Ld .tag: (ld_byteEn[tag]);
            tagged St .tag: (st_byteEn[tag]);
            default: ?;
        endcase);
    endmethod

    method ActionValue#(LSQHitInfo) getHit(LdStQTag t);
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
        ld_computed_enq[ld_enqP] <= False;
        ld_inIssueQ_enq[ld_enqP] <= False;
        ld_executing_enq[ld_enqP] <= False;
        ld_done_enq[ld_enqP] <= False;
        ld_killed_enq[ld_enqP] <= False;
        ld_readFrom_enq[ld_enqP] <= Invalid;
        ld_depLdQDeq_enq[ld_enqP] <= Invalid;
        ld_depStQDeq_enq[ld_enqP] <= Invalid;
`ifndef TSO_MM
        ld_depLdEx_enq[ld_enqP] <= Invalid;
        ld_depSBDeq_enq[ld_enqP] <= Invalid;
`endif
        ld_specTag_enq[ld_enqP] <= Invalid;
        ld_specBits_enq[ld_enqP] <= spec_bits;
        // don't touch wait wrong resp
        // Record older St. XXX We must use the up-to-date value st_valid;
        // otherwise, we may record a valid olderSt and never get it reset.
        StQTag olderSt = st_enqP == 0 ? fromInteger(valueof(StQSize) - 1)
                                      : (st_enqP - 1);
        if(st_valid_enq[olderSt]) begin
            ld_olderSt_enq[ld_enqP] <= Valid (olderSt);
            ld_olderStVerified_enq[ld_enqP] <= st_verified_enq[olderSt];
        end
        else begin
            ld_olderSt_enq[ld_enqP] <= Invalid;
            ld_olderStVerified_enq[ld_enqP] <= False;
        end
        // make conflict with incorrect spec
        wrongSpec_enq_conflict.wset(?);
    endmethod

    method Action enqSt(InstTag inst_tag,
                        MemInst mem_inst,
                        Maybe#(PhyDst) dst,
                        SpecBits spec_bits) if(st_can_enq_wire);
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
        st_computed_enq[st_enqP] <= False;
        st_verified_enq[st_enqP] <= False;
        st_specTag_enq[st_enqP] <= Invalid;
        st_specBits_enq[st_enqP] <= spec_bits;
        // make conflict with incorrect spec
        wrongSpec_enq_conflict.wset(?);
    endmethod

    method Action updateData(StQTag t, Data d);
        doAssert(st_valid_updData[t], "entry must be valid");
        doAssert(!st_computed_updData[t], "entry cannot be computed");
        st_stData_updData[t] <= d;
    endmethod

    method ActionValue#(LSQUpdateAddrResult) updateAddr(
        LdStQTag lsqTag, Addr pa, Bool mmio,
        ByteEn shift_be, Maybe#(SpecTag) spec_tag
    );
        // index vec for vector functions
        Vector#(LdQSize, LdQTag) idxVec = genWith(fromInteger);

        // We need to kill younger loads if lsqTag is a SQ entry, or we are
        // having a WEAK model. To reduce logic, we try to share the kill
        // search logic. But still need some logic specific to whether lsqTag
        // is an LQ or SQ entry.
        // Whether kill younger load or not
        Bool doKill = False;
        // Vector mask to indicate younger loads to check for kill
        Vector#(LdQSize, Bool) youngerLds = replicate(False);
        // The store virtual tag that will be compared with readFrom to
        // determine if a executing/done load has read a stale store. If a
        // load's readFrom is older than or *equal to* curSt, then the load
        // reads a stale value and should be killed. If curSt is invalid, then
        // the load should not be killed as long as readFrom is valid.  curSt
        // is in maybe type because of Ld killing Ld (in that case, curSt is
        // the olderSt field of the older Ld). "equal to" is also needed
        // because of Ld killing Ld.
        Maybe#(StQVirTag) curSt = Invalid;

        // update LQ/SQ entry and prepare for killing loads
        if(lsqTag matches tagged Ld .tag) begin
            // sanity check
            doAssert(ld_valid_updAddr[tag],
                     "updating entry must be valid");
            doAssert(!ld_computed_updAddr[tag] &&
                     !ld_inIssueQ_updAddr[tag] &&
                     !ld_executing_updAddr[tag] &&
                     !ld_done_updAddr[tag] &&
                     !ld_killed_updAddr[tag],
                     "updating entry should not be " +
                     "computed or issuing or executed or done or killed");
            doAssert(isValid(spec_tag) == (mmio || ld_memFunc[tag] == Ld),
                     "only Ld or MMIO needs to set spec tag");

            // write computed, paddr, shift be, specTag
            ld_computed_updAddr[tag] <= True;
            ld_paddr_updAddr[tag] <= pa;
            ld_isMMIO_updAddr[tag] <= mmio;
            ld_shiftedBE_updAddr[tag] <= shift_be;
            ld_specTag_updAddr[tag] <= spec_tag;

`ifndef TSO_MM
            // for WEAK model, try to kill younger load
            doKill = True;
            curSt = olderStVirTags[tag];
            LdQVirTag virTag = ldVirTags[tag];
            function Bool isYounger(LdQTag i);
                return ldVirTags[i] > virTag;
            endfunction
            youngerLds = map(isYounger, idxVec);
`endif
        end
        else if(lsqTag matches tagged St .tag) begin
            // sanity check
            doAssert(st_valid_updAddr[tag],
                     "updating entry must be valid");
            doAssert(!st_computed_updAddr[tag] && !st_verified_updAddr[tag],
                     "updating entry should not be computed or validated");
            doAssert(isValid(spec_tag) == mmio,
                     "only MMIO needs to set spec tag");

            // write computed, paddr, shift be, specTag
            st_computed_updAddr[tag] <= True;
            st_paddr_updAddr[tag] <= pa;
            st_isMMIO_updAddr[tag] <= mmio;
            st_shiftedBE_updAddr[tag] <= shift_be;
            st_specTag_updAddr[tag] <= spec_tag;

            // A store always try to kill younger loads
            doKill = True;
            StQVirTag virTag = stVirTags[tag];
            curSt = Valid (virTag);
            function Bool isYounger(LdQTag i);
                if(olderStVirTags[i] matches tagged Valid .t) begin
                    // load's immediate older store is younger than or equal to
                    // this updating entry, so load is younger
                    return t >= virTag;
                end
                else begin
                    // load has no older store, so load is older
                    return False;
                end
            endfunction
            youngerLds = map(isYounger, idxVec);
        end
        else begin
            doAssert(False, "unknown lsq tag");
        end

        // kill younger loads
        if(doKill) begin
            // Kill the youngested load which satisifies all the following
            // conditions:
            // (1) valid
            // (2) younger
            // (3) paddr & BE overlap with the updating Ld/Lr
            // (4) has read or is reading a stale value
            // We don't check computed or memFunc, because there is no pure fence
            // in LQ, and they are implied by executing bit
            function Bool needKill(LdQTag i);
                Bool valid = ld_valid_updAddr[i];
                Bool younger = youngerLds[i];
                Bool overlap = overlapAddr(pa, shift_be,
                                           ld_paddr_updAddr[i],
                                           ld_shiftedBE_updAddr[i]);
                // figure out if the load reads a stale value. Note that
                // checking executing bit is enough: every done load must also
                // have executing bit set.
                Bool read_stale;
                if(ld_executing_updAddr[i]) begin
                    if(readFromVirTags[i] matches tagged Valid .rf) begin
                        // younger load bypass from a store
                        if(curSt matches tagged Valid .st) begin
                            // if the forwarding store is not younger than the
                            // current store, then should kill
                            read_stale = rf <= st;
                        end
                        else begin
                            // this is the case of Ld killing Ld, the older Ld
                            // has no older store, so younger load is always
                            // safe
                            read_stale = False;
                        end
                    end
                    else begin
                        // This younger load reads memory, must be stale
                        read_stale = True;
                    end
                end
                else begin
                    // load is not done or executing, so cannot get stale value
                    read_stale = False;
                end
                // combine everything together
                return valid && younger && overlap && read_stale;
            endfunction
            Vector#(LdQSize, Bool) killLds = map(needKill, idxVec);
            if(findOldestLd(killLds) matches tagged Valid .killTag) begin
                ld_killed_updAddr[killTag] <= True;
                killByLdStQ.enq(ToSpecFifo {
                    data: killTag,
                    spec_bits: ld_specBits_updAddr[killTag]
                });
                // checks
                doAssert(ld_computed_updAddr[killTag], "must be computed");
                doAssert(!ld_isMMIO_updAddr[killTag], "cannot kill MMIO");
                doAssert(ld_memFunc[killTag] == Ld, "can only kill Ld");
                Maybe#(SpecTag) specTag = ld_specTag_updAddr[killTag];
                SpecBits specBits = ld_specBits_updAddr[killTag];
                doAssert(isValid(specTag), "killed Ld must have spec tag");
                doAssert(specBits[validValue(specTag)] == 1,
                         "sb of killed Ld has itself's spec tag");
                // when the Ld called incorrectSpeculation, it will kill
                // itself, and set waitWPResp if it is still executing at
                // that time
            end
        end

        // make conflict with incorrect spec
        wrongSpec_update_conflict.wset(?);

        // return waiting for wp resp bit: for deciding whether the updating Ld
        // can be issued
        return LSQUpdateAddrResult {
            waitWPResp: (case(lsqTag) matches
                tagged Ld .tag: (ld_waitWPResp_updAddr[tag]);
                default: False;
            endcase)
        };
    endmethod

    method ActionValue#(LSQIssueLdResult) issueLd(LdQTag tag,
                                                  Addr pa,
                                                  ByteEn shift_be,
                                                  SBSearchRes sbRes);
        doAssert(pa == ld_paddr_issue[tag], "Ld paddr incorrect");
        doAssert(shift_be == ld_shiftedBE_issue[tag], "Ld BE incorrect");
        doAssert(ld_valid_issue[tag], "issuing Ld must be valid");
        doAssert(ld_computed_issue[tag], "issuing Ld must be computed");
        doAssert(!ld_executing_issue[tag], "issuing Ld must not be executing");
        doAssert(!ld_done_issue[tag], "issuing Ld must not be done");
        doAssert(!ld_killed_issue[tag], "issuing Ld must not be killed");
        doAssert(ld_memFunc[tag] == Ld, "only issue Ld");
        doAssert(!ld_isMMIO_issue[tag], "issuing Ld cannot be MMIO");
        doAssert(
            !isValid(ld_depLdQDeq_issue[tag]) &&
`ifndef TSO_MM
            !isValid(ld_depLdEx_issue[tag]) &&
            !isValid(ld_depSBDeq_issue[tag]) &&
`endif
            !isValid(ld_depStQDeq_issue[tag]),
            "issuing entry should not have dependence"
        );
        doAssert(!ld_waitWPResp_issue[tag], "issuing Ld cannot wait for WP resp");

        // issue result
        LSQIssueLdResult issRes = Stall;

        // common thing for TSO and WEAK: valid SQ entry older than the load
        Maybe#(StQVirTag) precedingSt = olderStVirTags[tag];
        function Bool isValidOlderSt(StQTag i);
            Bool valid = st_valid_issue[i];
            Bool older;
            if(precedingSt matches tagged Valid .st) begin
                older = stVirTags[i] <= st;
            end
            else begin
                // load has no older store
                older = False;
            end
            return valid && older;
        endfunction
        Vector#(StQSize, Bool) validOlderSts = map(isValidOlderSt,
                                                   genWith(fromInteger));

`ifdef TSO_MM
        // TSO does not need to stall a load on fence or AMO. This is because
        // verfication of fence or AMO will be stalled until their fencing
        // effects are taken. And this will stall the verfication of younger
        // loads. The .rl->.aq ordering is also enforced by executing Lr/Sc/Amo
        // at sequential verification time (in RISC-V .aq and .rl can only be
        // found in Lr/Sc/Amo). TODO maybe it is better to use .aq as a hint
        // to throttle speculative load execution.

        // We only search for older overlapping SQ entries for bypass:
        // (1) valid older SQ entry
        // (2) computed
        // (3) overlap addr
        function Bool isOverlapSt(StQTag i);
            Bool valid_older = validOlderSts[i];
            Bool computed = st_computed_issue[i];
            Bool overlap = overlapAddr(pa, shift_be,
                                       st_paddr_issue[i],
                                       st_shiftedBE_issue[i]);
            return valid_older && computed && overlap;
        endfunction
        Vector#(StQSize, Bool) overlapSts = map(isOverlapSt,
                                                genWith(fromInteger));
        // search the youngest store, and derive issue result
        if(findYoungestSt(overlapSts) matches tagged Valid .stTag) begin
            // find an overlaping SQ entry, check its type
            case(st_memFunc[stTag])
                Sc, Amo: begin
                    // cannot forward, stall the load
                    issRes = Stall;
                    ld_depStQDeq_issue[tag] <= Valid (stTag);
                end
                St: begin
                    // check if forwarding is possible
                    if(be1CoverBe2(st_shiftedBE_issue[stTag], shift_be)) begin
                        // store covers the issuing load, forward
                        issRes = Forward (LSQForwardResult {
                            dst: ld_dst[tag],
                            data: st_stData_issue[stTag]
                        });
                        // set executing and record readFrom
                        ld_executing_issue[tag] <= True;
                        ld_readFrom_issue[tag] <= Valid (stTag);
                    end
                    else begin
                        // cannot forward, stall
                        issRes = Stall;
                        ld_depStQDeq_issue[tag] <= Valid (stTag);
                    end
                end
                default: begin
                    doAssert(False, "unknown st mem func");
                end
            endcase
        end
        else begin
            // no overlaping SQ entry is found, send to mem
            issRes = ToCache;
            // set executing and record readFrom
            ld_executing_issue[tag] <= True;
            ld_readFrom_issue[tag] <= Invalid;
            // check no SB in TSO
            doAssert(!isValid(sbRes.matchIdx) && !isValid(sbRes.forwardData),
                     "no SB in TSO");
        end

`else

        // WEAK model needs to do two searches
        // (1) Overlaping unissued Ld/Lr or fence in LQ (e.g., .aq in Lr)
        // (2) Overlaping St/Sc/Amo or fence in SQ (e.g., .aq in Amo)
        // XXX Strictly speaking, we should stall the load as long as there is
        // a fence. However, since we keep fence/Ld->St ordering, we can allow
        // a store younger than the fence to forward data to the load.

        // We need to check LQ entry which satisfies:
        // (1) valid
        // (2) older
        // (3) has acquire, or is computed but unissued and has overlap addr
        LdQVirTag issueVTag = ldVirTags[tag];
        function Bool isLdNeedCheck(LdQTag i);
            Bool valid = ld_valid_issue[i];
            Bool older = ldVirTags[i] < issueVTag;
            Bool acquire = ld_acq[i];
            Bool computed = ld_computed_issue[i];
            Bool unissued = !ld_executing_issue[i];
            Bool overlap = overlapAddr(pa, shift_be,
                                       ld_paddr_issue[i],
                                       ld_shiftedBE_issue[i]);
            return valid && older &&
                   (acquire || computed && unissued && overlap);
        endfunction
        Vector#(LdQSize, Bool) checkLds = map(isLdNeedCheck,
                                              genWith(fromInteger));
        Maybe#(LdQTag) matchLdTag = findYoungestLd(checkLds);

        // We need to check SQ entry which satisfies:
        // (1) valid and older
        // (2) has acquire, or is computed and has overlap addr
        function Bool isStNeedCheck(StQTag i);
            Bool valid_older = validOlderSts[i];
            Bool acquire = st_acq[i];
            Bool computed = st_computed_issue[i];
            Bool overlap = overlapAddr(pa, shift_be,
                                       st_paddr_issue[i],
                                       st_shiftedBE_issue[i]);
            return valid_older && (acquire || computed && overlap);
        endfunction
        Vector#(StQSize, Bool) checkSts = map(isStNeedCheck,
                                              genWith(fromInteger));
        Maybe#(StQTag) matchStTag = findYoungestSt(checkSts);

        // select the younger one from LQ and SQ search results
        LdQTag ldTag = validValue(matchLdTag);
        Maybe#(StQVirTag) ldTagOlderSt = olderStVirTags[ldTag];
        StQTag stTag = validValue(matchStTag);
        StQVirTag stVTag = stVirTags[stTag];
        if(isValid(matchLdTag) && (!isValid(matchStTag) ||
                                   (isValid(ldTagOlderSt) && 
                                    validValue(ldTagOlderSt) >= stVTag))) begin
            // stalled by Ld, Lr or acquire fence in LQ
            issRes = Stall;
            if(ld_acq[ldTag]) begin
                ld_depLdQDeq_issue[tag] <= matchLdTag;
            end
            else begin
                case(ld_memFunc[ldTag])
                    Ld: ld_depLdEx_issue[tag] <= matchLdTag;
                    Lr: ld_depLdQDeq_issue[tag] <= matchLdTag;
                    default: doAssert(False, "unknown ld func");
                endcase
            end
        end
        else if(isValid(matchStTag) && 
                (!isValid(matchLdTag) ||
                 !isValid(ldTagOlderSt) ||
                 validValue(ldTagOlderSt) < stVTag)) begin
            // bypass or stall by SQ
            if(st_acq[stTag]) begin
                // stall by acquire fence in SQ
                issRes = Stall;
                ld_depStQDeq_issue[tag] <= matchStTag;
            end
            else begin
                // match overlap Sc/Amo/St, check if forward is possible
                case(st_memFunc[stTag])
                    Sc, Amo: begin
                        // cannot forward, stall
                        issRes = Stall;
                        ld_depStQDeq_issue[tag] <= matchStTag;
                    end
                    St: begin
                        // check if forwarding is possible
                        if(be1CoverBe2(st_shiftedBE_issue[stTag],
                                       shift_be)) begin
                            // store covers the issuing load, forward
                            issRes = Forward (LSQForwardResult {
                                dst: ld_dst[tag],
                                data: st_stData_issue[stTag]
                            });
                            // set executing and record readFrom
                            ld_executing_issue[tag] <= True;
                            ld_readFrom_issue[tag] <= matchStTag;
                        end
                        else begin
                            // cannot forward, stall
                            issRes = Stall;
                            ld_depStQDeq_issue[tag] <= matchStTag;
                        end
                    end
                    default: begin
                        doAssert(False, "unknown st mem func");
                    end
                endcase
            end
        end
        else begin
            // nothing found in LQ or SQ, check SB search result
            if(sbRes.forwardData matches tagged Valid .d) begin
                // get forward from SB
                issRes = Forward (LSQForwardResult {
                    dst: ld_dst[tag],
                    data: d
                });
                // set executing and record readFrom. We view forwarding from
                // SB as reading memory, because the value is not in SQ
                ld_executing_issue[tag] <= True;
                ld_readFrom_issue[tag] <= Invalid;
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
                // set executing and record readFrom
                ld_executing_issue[tag] <= True;
                ld_readFrom_issue[tag] <= Invalid;
            end
        end

        // if the Ld is issued, remove dependences on this issue
        if(issRes != Stall) begin
            function Action setReady(LdQTag i);
            action
                // no need to check valid here, we can write anything to
                // invalid entry
                if(ld_depLdEx_issue[i] == Valid (tag)) begin
                    ld_depLdEx_issue[i] <= Invalid;
                end
            endaction
            endfunction
            Vector#(LdQSize, LdQTag) idxVec = genWith(fromInteger);
            joinActions(map(setReady, idxVec));
        end
`endif

        // make conflict with incorrect spec
        wrongSpec_issue_conflict.wset(?);

        return issRes;
    endmethod

    method ActionValue#(LSQIssueLdInfo) getIssueLd;
        issueLdQ.deq;
        // reset inIssueQ
        let tag = issueLdQ.first.data.tag;
        ld_inIssueQ_issue[tag] <= False;
        doAssert(ld_inIssueQ_issue[tag], "Ld should be in issueQ");
        doAssert(ld_memFunc[tag] == Ld, "must be Ld");
        return issueLdQ.first.data;
    endmethod

    method ActionValue#(LSQKillLdInfo) getLdKilledByLdSt;
        killByLdStQ.deq;
        let tag = killByLdStQ.first.data;
        doAssert(
            ld_valid_getKill[tag] &&
            ld_computed_getKill[tag] &&
            ld_killed_getKill[tag] &&
            ld_executing_getKill[tag] &&
            ld_memFunc[tag] == Ld,
            "killed load must be valid, computed, killed and executing/done"
        );
        return LSQKillLdInfo {
            instTag: ld_instTag[tag],
            specTag: ld_specTag_getKill[tag]
        };
    endmethod

`ifdef TSO_MM
    method ActionValue#(LSQKillLdInfo) getLdKilledByCache;
        killByCacheQ.deq;
        let tag = killByCacheQ.first.data;
        doAssert(
            ld_valid_getKill[tag] &&
            ld_computed_getKill[tag] &&
            ld_killed_getKill[tag] &&
            ld_executing_getKill[tag] &&
            ld_memFunc[tag] == Ld,
            "killed load must be valid, computed, killed and executing/done"
        );
        return LSQKillLdInfo {
            instTag: ld_instTag[tag],
            specTag: ld_specTag_getKill[tag]
        };
    endmethod
`endif

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
            specTag: ld_specTag_deqLd[deqP],
            specBits: ld_specBits_deqLd[deqP],
            waitWPResp: ld_waitWPResp_deqLd[deqP]
        };
    endmethod

    method Action deqLd if(deqLdGuard);
        LdQTag deqP = ld_deqP_deqLd;

        // sanity check
        doAssert(checkAddrAlign(ld_paddr_deqLd[deqP], ld_byteEn[deqP]),
                 "addr BE should be naturally aligned");
        doAssert(!ld_waitWPResp_deqLd[deqP],
                 "cannot wait for wrong path resp");
        if(ld_memFunc[deqP] == Ld || ld_isMMIO_deqLd[deqP]) begin
            doAssert(isValid(ld_specTag_deqLd[deqP]),
                     "Ld or MMIO must have spec tag");
            SpecTag specTag = validValue(ld_specTag_deqLd[deqP]);
            if(ld_isMMIO_deqLd[deqP]) begin
                doAssert(ld_specBits_deqLd[deqP] == (1 << specTag),
                         "spec bits of MMIO must be just itself's spec tag");
            end
            else begin
                doAssert(ld_specBits_deqLd[deqP][specTag] == 1,
                         "spec bits of non-MMIO Ld must contain itself");
            end
        end
        else begin
            doAssert(!isValid(ld_specTag_deqLd[deqP]),
                     "non-MMIO Lr cannot have spec tag");
            doAssert(ld_specBits_deqLd[deqP] == 0,
                     "non-MMIO Lr must have zero spec bits");
        end

        // remove the entry
        ld_valid_deqLd[deqP] <= False;
        ld_deqP_deqLd <= getNextLdPtr(deqP);

        // wakeup loads stalled by this entry
        function Action setReady(LdQTag i);
        action
            // no need to check valid, we can write anything to invalid entry
            if(ld_depLdQDeq_deqLd[i] == Valid (deqP)) begin
                ld_depLdQDeq_deqLd[i] <= Invalid;
            end
        endaction
        endfunction
        Vector#(LdQSize, LdQTag) idxVec = genWith(fromInteger);
        joinActions(map(setReady, idxVec));

        // make conflict with incorrect spec
        wrongSpec_deqLd_conflict.wset(?);
    endmethod

    method StQDeqEntry firstSt if(deqStGuard);
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
            specTag: st_specTag_deqSt[deqP],
            specBits: st_specBits_deqSt[deqP]
        };
    endmethod

    method Action deqSt if(deqStGuard);
        StQTag deqP = st_deqP;

        // sanity check
        doAssert(checkAddrAlign(st_paddr_deqSt[deqP], st_byteEn[deqP]),
                 "addr BE should be naturally aligned");
        if(st_isMMIO_deqSt[deqP]) begin
            doAssert(isValid(st_specTag_deqSt[deqP]),
                     "Ld or MMIO must have spec tag");
            SpecTag specTag = validValue(st_specTag_deqSt[deqP]);
            doAssert(st_specBits_deqSt[deqP] == (1 << specTag),
                     "spec bits of MMIO must be just itself's spec tag");
        end
        else begin
            doAssert(!isValid(st_specTag_deqSt[deqP]),
                     "non-MMIO St/Sc/Amo cannot have spec tag");
            doAssert(st_specBits_deqSt[deqP] == 0,
                     "non-MMIO St/Sc/Amo must have zero spec bits");
        end

        // remove entry
        st_valid_deqSt[deqP] <= False;
        st_deqP <= getNextStPtr(deqP);

        // tell LQ entries that this SQ entry is removed, no need to check LQ
        // entry valid:
        // (1) reset olderSt
        // (2) reset readFrom
        // (3) reset depStQDeq
        function Action resetSt(LdQTag i);
        action
            if(ld_olderSt_deqSt[i] == Valid (deqP)) begin
                ld_olderSt_deqSt[i] <= Invalid;
                // no need to change ld_olderStVerified, it is only meaningful
                // when ld_olderSt is valid
            end
            if(ld_readFrom_deqSt[i] == Valid (deqP)) begin
                ld_readFrom_deqSt[i] <= Invalid;
            end
            if(ld_depStQDeq_deqSt[i] == Valid (deqP)) begin
                ld_depStQDeq_deqSt[i] <= Invalid;
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
        // kill a load if it satisfies the following conditions:
        // (1) valid
        // (2) executing and read from memory (just killing done loads is not
        // enough, because there is a delay from getting value and marking
        // done)
        // (3) addr overlap
        // We don't check computed or memFunc, because there is no pure fence
        // in LQ, and they are implied by executing
        function Bool needKill(LdQTag i);
            Bool valid = ld_valid_evict[i];
            Bool executing = ld_executing_evict[i];
            Bool read_mem = !isValid(ld_readFrom_evict[i]);
            Bool overlap = getLineAddr(ld_paddr_evict[i]) == lineAddr;
            return valid && executing && read_mem && overlap;
        endfunction

        // kill the oldest load
        Vector#(LdQSize, Bool) killLds = map(needKill, genWith(fromInteger));
        if(findOldestLd(killLds) matches tagged Valid .killTag) begin
            ld_killed_evict[killTag] <= True;
            killByCacheQ.enq(ToSpecFifo {
                data: killTag,
                spec_bits: ld_specBits_evict[killTag]
            });
            // checks
            doAssert(ld_computed_evict[killTag], "must be computed");
            doAssert(!ld_isMMIO_evict[killTag], "cannot kill MMIO");
            doAssert(ld_memFunc[killTag] == Ld, "can only kill Ld");
            Maybe#(SpecTag) specTag = ld_specTag_evict[killTag];
            SpecBits specBits = ld_specBits_evict[killTag];
            doAssert(isValid(specTag), "killed Ld must have spec tag");
            doAssert(specBits[validValue(specTag)] == 1,
                     "sb of killed Ld has itself's spec tag");
            // when the Ld called incorrectSpeculation, it will kill
            // itself, and set waitWPResp if it is still executing at
            // that time
        end

        // make conflict with incorrect spec
        wrongSpec_cacheEvict_conflict.wset(?);
    endmethod

`else

    method Action wakeupLdStalledBySB(SBIndex sbIdx);
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

    interface SpeculationUpdate specUpdate;
        method Action correctSpeculation(SpecBits mask);
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

            // clear spec bits for issueQ and killQ
            issueLdQ.specUpdate.correctSpeculation(mask);
            killByLdStQ.specUpdate.correctSpeculation(mask);
`ifdef TSO_MM
            killByCacheQ.specUpdate.correctSpeculation(mask);
`endif
        endmethod

        method Action incorrectSpeculation(SpecTag specTag);
            // idx vec

            // clear wrong path LQ entries & set wrong path load filter. NOTE
            // that olderSt and olderStVerified fields are not affected by the
            // kill
            function Action killLdQ(LdQTag i);
            action
                if(ld_specBits_wrongSpec[i][specTag] == 1) begin
                    ld_valid_wrongSpec[i] <= False;
                    // set wrong path load resp filter
                    if (ld_valid_wrongSpec[i] &&
                        ld_executing_wrongSpec[i] &&
                        !ld_done_wrongSpec[i]) begin
                        ld_waitWPResp_wrongSpec[i] <= True;
                        doAssert(ld_memFunc[i] == Ld,
                                 "only load resp can be wrong path");
                    end
                end
            endaction
            endfunction
            Vector#(LdQSize, LdQTag) ldIdxVec = genWith(fromInteger);
            joinActions(map(killLdQ, ldIdxVec));

            // clear wrong path SQ entries
            function Action killStQ(StQTag i);
            action
                if(st_specBits_wrongSpec[i][specTag] == 1) begin
                    st_valid_wrongSpec[i] <= False;
                end
            endaction
            endfunction
            Vector#(StQSize, StQTag) stIdxVec = genWith(fromInteger);
            joinActions(map(killStQ, stIdxVec));

            // kill entries in issueQ and killQ
            issueLdQ.specUpdate.incorrectSpeculation(specTag);
            killByLdStQ.specUpdate.incorrectSpeculation(specTag);
`ifdef TSO_MM
            killByCacheQ.specUpdate.incorrectSpeculation(specTag);
`endif

            // change enqP: make valid entries always consecutive: new enqP is
            // the oldest **VALID** entry that gets killed. If such entry does
            // not exists, then enqP remains the same.
            // LdQ enqP
            function Bool isValidLdKilled(LdQTag i);
                return ld_valid_wrongSpec[i] &&
                       ld_specBits_wrongSpec[i][specTag] == 1;
            endfunction
            Vector#(LdQSize, Bool) killedValidLds = map(isValidLdKilled,
                                                        ldIdxVec);
            if(findOldestLd(killedValidLds) matches tagged Valid .t) begin
                ld_enqP <= t;
            end
            // StQ enqP
            function Bool isValidStKilled(StQTag i);
                return st_valid_wrongSpec[i] &&
                       st_specBits_wrongSpec[i][specTag] == 1;
            endfunction
            Vector#(StQSize, Bool) killedValidSts = map(isValidStKilled,
                                                        stIdxVec);
            StQTag new_st_enqP = st_enqP;
            if(findOldestSt(killedValidSts) matches tagged Valid .t) begin
                new_st_enqP = t;
            end
            st_enqP <= new_st_enqP;

            // change SQ verifyP: new verifyP is the oldest entry that is
            // neither killed nor verified. If such entry does not exists, then
            // verifyP should be the same as new enqP.
            function Bool unkilledUnverified(StQTag i);
                return st_valid_wrongSpec[i] &&
                       st_specBits_wrongSpec[i][specTag] != 1 &&
                       !st_verified_wrongSpec[i];
            endfunction
            Vector#(StQSize, Bool) unverifiedSts = map(unkilledUnverified,
                                                       stIdxVec);
            if(findOldestSt(unverifiedSts) matches tagged Valid .t) begin
                st_verifyP_wrongSpec <= t;
            end
            else begin
                st_verifyP_wrongSpec <= new_st_enqP;
            end

            // make conflict with others
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
        endmethod
    endinterface
endmodule
