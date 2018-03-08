
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
    // Ld has got its result
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
    // Ld stalled by unexecuted Ld to same addr, should wait it to be issued
    Maybe#(LdQTag)   depLdEx;
    // Ld stalled by St/Sc/Amo in SQ to same addr, wait for it to deq
    Maybe#(StQTag)   depStQDeq;
`ifndef TSO_MM
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

typedef union tagged {
    LdQTag Ld;
    StQTag St;
} LdStQTag deriving(Bits, Eq, FShow);

interface SplitLSQ;
    // Enq at renaming. We split to 2 enq methods to enable synthesize
    // boundary. If we merge into 1 enq method, the guard will depend on the
    // type of mem inst, so cannot be synthesized.
    method Maybe#(LdStQTag) enqTag(MemFunc f);
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
    // update store data (shifted for St and Sc, unshifted for AMO)
    method Action updateData(StQTag t, Data d);
    // Update addr after address translation, and set the spec tag if it is a
    // load or MMIO. Also search for the (oldest) younger load to kill. Return
    // if the entry is waiting for wrong path resp.
    method ActionValue#(LSQUpdateAddrResult) updateAddr(
        LdStQTag lsqTag, Addr paddr, Bool isMMIO,
        ByteEn shiftedBE, Maybe#(SpecTag) specTag
    );
    // Issue a load, and remove dependence on this load issue.
    method ActionValue#(LSQIssueResult) issueLd(
        LdStQTag lsqTag, Addr paddr, ByteEn shiftedBE, SBSearchRes sbRes
    );
    // Get the load to issue
    method ActionValue#(LSQIssueLdInfo) getIssueLd;
    // Get the load killed by ld/st ordering
    method ActionValue#(LSQKillLdInfo) getLdKilledByLdSt;
    // Get load resp
    method ActionValue#(LSQRespLdResult) respLd(LdStQTag t, Data alignedData);
    // Deq LQ entry, and wakeup stalled loads. The guard only checks that entry
    // is valid and all older stores have got their addrs. Outside world should
    // do the following:
    // (1) sends Lr or MMIO to cache only at deq port
    // (2) check specBits & waitWPResp before issue Lr or MMIO
    // (3) check specBits & status bits before deq
    // (4) check killed before (if killed, cannot deq and then commit ROB)
    // (5) set ROB entry of deq mem inst to Executed (so that ROB can commit)
    // (6) when deq Ld, clear spectag of Ld globally
    method LdQDeqEntry firstLd;
    method Action deqLd;
    // Validate SQ entry sequentially to be non-speculative (TSO only)
    method Action validateSt;
    // Deq SQ entry, and wakeup stalled loads. Also change the readFrom and
    // olderSt fields of loads.
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

function Bool isStQMemFunc(MemFunc);
    return (case(f)
        St, Sc, Amo: (True);
        default: (False);
    endcase);
endfunction

// extract vector ports from vector of EHRs
function Vector#(n, Reg#(t)) getVEhrPort(Vector#(n, Ehr#(m, t)) ehrs, Integer p);
    function Reg#(t) get(Ehr#(m, t) e) = e[p];
    return map(get, ehrs);
endfunction

// issueQ of LSQ tags for issue
typedef SpecFifo_SB_deq_enq_SB_deq_wrong_C_enq#(2, LSQIssueInfo) LSQIssueLdQ;
(* synthesize *)
module mkLSQIssueLdQ(LSQIssueLdQ);
    let m <- mkSpecFifo_SB_deq_enq_SB_deq_wrong_C_enq(True);
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
    // ordering
    // getHit, getKillLd, findIssue <
    // (deqLd C verifySt (TSO only)) <
    // cacheEvict <
    // updateAddr <
    // issueLd, getIssueLd <
    // enqIssueQ <
    // wakeupLdStalledBySB (Weak only) <
    // deqSt <
    // respLd <
    // updateData <
    // enq <
    // correctSpec

    // Scheduling notes:
    // - getKilled, deqLd, validateSt are almost readonly, so put them at
    // beginning.
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
    // wakeupLdStalledBySB < issueLd. However, since issueLd sets depSBDeq,
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
    // never be activated in reality. Another (non-important) reason to put
    // updateAddr < respLd is that respLd set done bit, which are used by
    // updateAddr in associative search. TODO We can make a bypass wire to read
    // paddr[0] in respLd method.
    // - There is a bypassing path from validatedSt to deqSt, i.e., a store
    // can be validated and issued to memory at the same cycle. This is
    // probably a desirable behavior.
    // - We put findIssue < updateAddr to prevent findIssue insert a newly
    // updated LQ entry into issueQ; this can create problem because the new
    // updated load may be immediately issued to execution in the same cycle
    // later.
    // - Only one of deqLd and verifySt can fire at one cycle.

    // W.r.t wrongSpec:
    // getKillLd < wrongSpec (they fire in same rule)
    // findIss < wrongSpec (findIss is read only)
    // All other methods or rules that have conflicting accesses with wrongSpec
    // should conflict with wrongSpec to cut off any possible bypass path.

    Integer valid_getKill_port = 0;
    Integer valid_findIss_port = 0;
    Integer valid_wrongSpec_port = 0;
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

    // LQ
    // entry valid bits
    Vector#(LdQSize, Ehr#(3, Bool))             ld_valid      <- replicateM(mkEhr(False));
    // entry contents
    Vector#(LdQSize, Reg#(InstTag))             ld_instTag    <- replicateM(mkRegU);
    Vector#(LdQSize, Reg#(LdQMemFunc))          ld_memFunc    <- replicateM(mkRegU);
    Vector#(LdQSize, Reg#(Bool))                ld_unsigned   <- replicateM(mkRegU);
    Vector#(LdQSize, Reg#(ByteEn))              ld_byteEn     <- replicateM(mkRegU);
    Vector#(LdQSize, Reg#(Bool))                ld_acq        <- replicateM(mkRegU);
    Vector#(LdQSize, Reg#(Bool))                ld_rel        <- replicateM(mkRegU);
    Vector#(LdQSize, Reg#(Maybe#(PhyDst)))      ld_dst        <- replicateM(mkRegU);
    Vector#(LdQSize, Ehr#(3, Addr))             ld_paddr      <- replicateM(mkEhr(?));
    Vector#(LdQSize, Ehr#(3, Bool))             ld_isMMIO     <- replicateM(mkEhr(?));
    Vector#(LdQSize, Ehr#(3, ByteEn))           ld_shiftedBE  <- replicateM(mkEhr(?));
    Vector#(LdQSize, Ehr#(3, Bool))             ld_computed   <- replicateM(mkEhr(?));
    Vector#(LdQSize, Ehr#(3, Bool))             ld_inIssueQ   <- replicateM(mkEhr(?));
    Vector#(LdQSize, Ehr#(3, Bool))             ld_executing  <- replicateM(mkEhr(?));
    Vector#(LdQSize, Ehr#(3, Bool))             ld_done       <- replicateM(mkEhr(?));
    Vector#(LdQSize, Ehr#(3, Bool))             ld_killed     <- replicateM(mkEhr(?));
    Vector#(LdQSize, Ehr#(3, Maybe#(StQTag)))   ld_olderSt    <- replicateM(mkEhr(?));
    Vector#(LdQSize, Ehr#(3, Maybe#(StQTag)))   ld_readFrom   <- replicateM(mkEhr(?));
    Vector#(LdQSize, Ehr#(3, Maybe#(LdStQTag))) ld_depLdQDeq  <- replicateM(mkEhr(?));
    Vector#(LdQSize, Ehr#(3, Maybe#(LdStQTag))) ld_depLdEx    <- replicateM(mkEhr(?));
    Vector#(LdQSize, Ehr#(3, Maybe#(SBIndex)))  ld_depStQDeq  <- replicateM(mkEhr(?));
`ifndef TSO_MM
    Vector#(LdQSize, Ehr#(3, Maybe#(SBIndex)))  ld_depSBDeq   <- replicateM(mkEhr(?));
`endif
    Vector#(LdQSize, Ehr#(3, Maybe#(SpecTag)))  ld_specTag    <- replicateM(mkEhr(?));
    Vector#(LdQSize, Ehr#(3, SpecBits))         ld_specBits   <- replicateM(mkEhr(?));
    // wrong-path load filter (must init to all False)
    Vector#(LdStQSize, Reg#(Bool))              ld_waitWPResp <- replicateM(mkReg(False));
    // enq/deq ptr
    Reg#(LdQTag)    ld_enqP <- mkReg(0);
    Ehr#(2, LdQTag) ld_deqP <- mkEhr(0);

    // Ports of each EHR used in each method or rule
    let ld_valid_getKill   = getVEhrPort(ld_valid, 0);
    let ld_valid_findIss   = getVEhrPort(ld_valid, 0);
    let ld_valid_wrongSpec = getVEhrPort(ld_valid, 0);
    let ld_valid_deq       = getVEhrPort(ld_valid, 0); // write
    let ld_valid_evict     = getVEhrPort(ld_valid, 1);
    let ld_valid_updAddr   = getVEhrPort(ld_valid, 1);
    let ld_valid_issue     = getVEhrPort(ld_valid, 1);
    let ld_valid_enqIss    = getVEhrPort(ld_valid, 1); // assert
    let ld_valid_respLd    = getVEhrPort(ld_valid, 1); // assert
    let ld_valid_enq       = getVEhrPort(ld_valid, 1); // write

    // SQ
    // entry valid bits
    Vector#(LdQSize, Ehr#(3, Bool))            st_valid     <- replicateM(mkEhr(False));
    // entry contents
    Vector#(LdQSize, Reg#(InstTag))            st_instTag   <- replicateM(mkRegU);
    Vector#(LdQSize, Reg#(StQMemFunc))         st_memFunc   <- replicateM(mkRegU);
    Vector#(LdQSize, Reg#(ByteEn))             st_byteEn    <- replicateM(mkRegU);
    Vector#(LdQSize, Reg#(Bool))               st_acq       <- replicateM(mkRegU);
    Vector#(LdQSize, Reg#(Bool))               st_rel       <- replicateM(mkRegU);
    Vector#(LdQSize, Reg#(Maybe#(PhyDst)))     st_dst       <- replicateM(mkRegU);
    Vector#(LdQSize, Ehr#(3, Addr))            st_paddr     <- replicateM(mkEhr(?));
    Vector#(LdQSize, Ehr#(3, Bool))            st_isMMIO    <- replicateM(mkEhr(?));
    Vector#(LdQSize, Ehr#(3, ByteEn))          st_shiftedBE <- replicateM(mkEhr(?));
    Vector#(LdQSize, Ehr#(3, Data))            st_stData    <- replicateM(mkEhr(?));
    Vector#(LdQSize, Ehr#(3, Bool))            st_computed  <- replicateM(mkEhr(?));
    Vector#(LdQSize, Ehr#(3, Maybe#(SpecTag))) st_specTag   <- replicateM(mkEhr(?));
    Vector#(LdQSize, Ehr#(3, SpecBits))        st_specBits  <- replicateM(mkEhr(?));
    // enq/deq ptr
    Reg#(StQTag)    st_enqP <- mkReg(0);
    Ehr#(2, StQTag) st_deqP <- mkEhr(0);
`ifdef TSO_MM
    // For TSO only: store-verify-pointer. To make our OOO execution conform to
    // TSO, we need to verify each load and store sequentially to mimic TSO I2E
    // operational model. Besides sequential ordering of verification, a Ld can
    // be verified when it has its result, a St can be verified when it has
    // addr and data computed, an Lr/Sc/Amo can be verified when it has
    // completed memory access, and a fence can be verified when all older mem
    // accesses have been dequeued from LSQ. A Ld or Lr is verified by
    // dequeuing it from LQ, so there is no verify-pointer in LQ. A
    // Sc/Amo/Fence is also verfied by dequeuing it from SQ. The only reason
    // for needing this verify-pointer in SQ is that a verified St can still
    // live in SQ. All stores within [deqP, verify-pointer) are Sts that have
    // been verfied, and verify-pointer points to the next store to be verfied.
    // This pointer helps enforcing the sequential verification ordering:
    // - The LQ head can be dequeued when its olderSt is invalid or older than
    // SQ verify pointer.
    // - The SQ entry at verify pointer can be verified when the LQ head's
    // olderSt is younger than or equal to this SQ entry.
    Ehr#(2, StQTag) st_verifyP <- mkEhr(0);
`endif

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
    RWire#(void) wrongSpec_hit_conflict <- mkRWire;
    RWire#(void) wrongSpec_enqIss_conflict <- mkRWire;
    RWire#(void) wrongSpec_enq_conflict <- mkRWire;
    RWire#(void) wrongSpec_update_conflict <- mkRWire;
    RWire#(void) wrongSpec_issue_conflict <- mkRWire;
    RWire#(void) wrongSpec_respLd_conflict <- mkRWire;
    RWire#(void) wrongSpec_deq_conflict <- mkRWire;
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
    // virtual tag for st verify ptr. XXX NOTE that this ptr may point to
    // invalid entry, so need to check entry valid first.
    StQVirTag stVerifyVirTag = getStVirTag(st_verifyP);

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
        Vector#(StQSize, StQTag) idxVec = genWith(fromInteger);
        LdStQTag tag = fold(getYounger, idxVec);
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

    // virtual tags for olderSt port 0
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

    // virtual tags for readFrom port 0
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
                !isValid(ld_depLdEx_findIss[i]) &&
`ifndef TSO_MM
                !isValid(ld_depSBDeq_findIss[i]) &&
`endif
                !isValid(ld_depStQDeq_findIss[i]) && // (5) no dependency
                !waitWPResp[i] && // (6) not wating wrong path resp
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
                paddr: paddr_findIss[tag],
                shiftedBE: shiftedBE_findIss[tag]
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
        doAssert(!waitWPResp[info.tag],
                 "enq issueQ entry cannot wait for wrong path resp");
        doAssert(!ld_isMMIO_enqIss[info.tag],
                 "enq issueQ entry cannot be MMIO");
        doAssert(!isValid(ld_depLdQDeq_enqIss[info.tag]) &&
                 !isValid(ld_depLdEx_enqIss[info.tag]) &&
`ifndef TSO_MM
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
        ld_inIssueQ[info.tag] <= True;
        // make conflict with incorrect spec
        wrongSpec_enqIss_conflict.wset(?);
    endrule

`ifdef BSIM
    // Sanity check in simulation. All valid entry are consective within deqP
    // and enqP, outsiders are invalid entries
    (* fire_when_enabled, no_implicit_conditions *)
    rule checkLdQValid;
        Bool allEmpty = all( \== (False),  readVEhr(0, ld_valid) );
        if(allEmpty) begin
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
                        "valid entries must be within deqP and enqP");
            end
        end
    endrule

    (* fire_when_enabled, no_implicit_conditions *)
    rule checkStQValid;
        Bool allEmpty = all( \== (False), readVEhr(0, st_valid) ),
        function Bool in_range(StQTag i);
            // if i is within deqP and enqP, it should be valid
            if(allEmpty) begin
                return False;
            end
            else begin
                if(st_deqP[0] < st_enqP) begin
                    return st_deqP[0] <= i && i < st_enqP;
                end
                else begin
                    return st_deqP[0] <= i || i < st_enqP;
                end
            end
        endfunction
        for(Integer i = 0; i < valueof(StQSize); i = i+1) begin
            doAssert(in_range(fromInteger(i)) == st_valid[i][0],
                     "valid entries must be within deqP and enqP");
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

    method ByteEn getOrigBE(LdStQTag t);
        return (case(t) matches
            tagged Ld .tag: (ld_byteEn[tag]);
            tagged St .tag: (st_byteEn[tag]);
            default: ?;
        endcase);
    endmethod

    method ActionValue#(LSQHitInfo) getHit(LdStQTag t);
        wrongSpec_hit_conflict.wset(?); // TODO remove, seems useless
        return (case(t) matches
            tagged Ld .tag: (LSQHitInfo {
                waitWPResp: ld_waitWPResp[tag],
                dst: ld_dst[tag]
            });
            tagged St .tag: (LSQHitInfo {
                waitWPResp: False,
                dst: st_dst[tag]
            });
            default: ?;
        endcase);
    endmethod

    method Maybe#(LdStQTag) enqTag(MemFunc f);
        return (case(f)
            Ld, Lr:  (ld_can_enq_wire ? Valid (Ld (ld_enqP)) : Invalid);
            default: (st_can_enq_wire ? Valid (St (st_enqP)) : Invalid);
        endcase);
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
        ld_depLdEx_enq[ld_enqP] <= Invalid;
        ld_deqStQDeq_enq[ld_enqP] <= Invalid;
`ifndef TSO_MM
        ld_depSBDeq_enq[ld_enqP] <= Invalid;
`endif
        ld_specTag_enq[ld_enqP] <= Invalid;
        ld_specBits_enq[ld_enqP] <= spec_bits;
        // don't touch wait wrong resp
        // Record older St. XXX We must use the up-to-date value st_valid;
        // otherwise, we may record a valid olderSt and never get it reset.
        StQTag olderSt = st_enqP == 0 ? fromInteger(valueof(StQSize) - 1)
                                      : (st_enqP - 1);
        ld_olderSt_enq[ld_enqP] = st_valid_enq[olderSt] ? Valid (olderSt)
                                                        : Invalid;
        // make conflict with incorrect spec
        wrongSpec_enq_conflict.wset(?);
    endmethod

    method Action enqSt(InstTag inst_tag,
                        MemInst mem_inst,
                        Maybe#(PhyDst) dst,
                        SpecBits spec_bits) if(st_can_enq_wire);
        doAssert(!st_valid_enq[ld_enqP],
                 "entry at enqP must be invalid");
        doAssert(isStQMemFunc(mem_inst.mem_func),
                 "must be StQ mem func");
        // set entry valid and move ptr
        st_valid_enq[ld_enqP] <= True;
        st_enqP <= getNextStPtr(st_enqP);
        // set up the entry
        st_instTag[st_enqP] <= inst_tag;
        st_memFunc[st_enqP] <= getStQMemFunc(mem_inst.mem_func);
        st_byteEn[st_enqP] <= mem_inst.byteEn;
        st_acq[st_enqP] <= mem_inst.aq;
        st_rel[st_enqP] <= mem_inst.rl;
        st_dst[st_enqP] <= dst;
        st_computed_enq[st_enqP] <= False;
        st_validated_enq[st_enqP] <= False;
        st_specTag_enq[st_enqP] <= Invalid;
        st_specBits_enq[st_enqP] <= spec_bits;
        // make conflict with incorrect spec
        wrongSpec_enq_conflict.wset(?);
    endmethod

    method Action updateData(StQTag t, Data d);
        doAssert(st_valid_updData[t], "entry must be valid");
        doAssert(!st_computed_updData[t], "entry cannot be computed");
        doAssert(!st_validated_updData[t], "entry cannot be validated");
        st_stData[t] <= d;
    endmethod

    method ActionValue#(LSQUpdateResult) updateAddr(LdStQTag lsqTag,
                                                    Addr pa,
                                                    Bool mmio,
                                                    ByteEn shift_be,
                                                    Maybe#(SpecTag) spec_tag);
        // index vec for vector functions
        Vector#(LdStQSize, LdStQTag) idxVec = genWith(fromInteger);

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
        // the olderSt field of the olde Ld). "equal to" is also needed because
        // of Ld killing Ld.
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
            // for WEAK model, kill younger load
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
            doAssert(!st_computed_updAddr[tag] && !st_validated_updAddr[tag],
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
            // (2) computed
            // (3) younger
            // (4) paddr & BE overlap with the updating Ld/Lr
            // (5) has read or is reading a stale value
            function Bool needKill(LdQTag i);
                Bool valid = ld_valid_updAddr[i];
                Bool computed = ld_computed_updAddr[i];
                Bool younger = youngerLds[i];
                Bool overlap = overlapAddr(pa, shift_be,
                                           ld_paddr_updAddr[i],
                                           ld_shiftedBE_updAddr[i]);
                // figure out if the load reads a stale value
                Bool read_stale;
                if(ld_done_updAddr[i] || ld_executing_updAddr[i]) begin
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
                return valid && computed && younger && overlap && read_stale;
            endfunction
            Vector#(LdQSize, Bool) killLds = map(needKill, idxVec);
            if(findOldestLd(killLds) matches tagged Valid .killTag) begin
                ld_killed_updAddr[killTag] <= True;
                killByLdStQ.enq(ToSpecFifo {
                    data: killTag,
                    spec_bits: ld_specBits_updAddr[killTag]
                });
                // checks
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
        return LSQUpdateResult {
            waitWPResp: (case(lsqTag) matches
                tagged Ld .tag: (ld_waitWPResp[tag]);
                default: False;
            endcase);
        };
    endmethod

    method ActionValue#(LSQIssueResult) issueLd(LdQTag tag,
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
            !isValid(ld_depLdEx_issue[tag]) &&
`ifndef TSO_MM
            !isValid(ld_depSBDeq_issue[tag]) &&
`endif
            !isValid(ld_depStQDeq_issue[tag]),
            "issuing entry should not have dependence"
        );
        doAssert(!ld_waitWPResp[tag], "issuing Ld cannot wait for WP resp");

        // issue result
        LSQIssueResult issRes = Stall;

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
                    if(be1CoverBe2(st_shiftedBE_issue[stTag], shifted_be)) begin
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

        // WEAK model needs to do two searches (1) Overlaping unissued Ld/Lr or
        // fence in LQ (e.g., .aq in Lr) (2) Overlaping St/Sc/Amo or fence in
        // SQ (e.g., .aq in Amo) XXX Strictly speaking, we should stall the
        // load as long as there is a fence. However, since we keep
        // fence/Ld->St ordering, we can allow a store younger than the fence
        // to forward data to the load.

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
                case(st_memFunc[validValue[stTag]])
                    Sc, Amo: begin
                        // cannot forward, stall
                        issRes = Stall;
                        ld_depStQDeq_issue[tag] <= matchStTag;
                    end
                    St: begin
                        // check if forwarding is possible
                        if(be1CoverBe2(st_shiftedBE_issue[stTag],
                                       shifted_be)) begin
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
`endif

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
        return issueQ.first.data;
    endmethod

    method ActionValue#(LSQKillLdInfo) getKillLd;
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
            instTag: ld_instTag[lsqTag],
            specTag: ld_specTag_kill[lsqTag]
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
