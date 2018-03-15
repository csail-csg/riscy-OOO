import Types::*;
import ProcTypes::*;
import TestTypes::*;
import SatDownCounter::*;
import Vector::*;
import HasSpecBits::*;
import SpecFifo::*;
import Randomizable::*;
import Ehr::*;

// simply model the delay in an OOO TLB
// but we need to kill wrong-path req

typedef struct {
    TestId testId;
    LdStQTag lsqTag;
    SpecTag specTag;
} DelayTLBReq deriving(Bits, Eq, FShow);

interface DelayTLB;
    method Action procReq(DelayTLBReq r, SpecBits sb);
    method DelayTLBReq respVal;
    method Action deqResp;
    interface SpeculationUpdate specUpdate;
endinterface

typedef SpecFifo_SB_deq_enq_SB_deq_wrong_C_enq#(2, DelayTLBReq) TLBRespQ;
(* synthesize *)
module mkTLBRespQ(TLBRespQ);
    let m <- mkSpecFifo_SB_deq_enq_SB_deq_wrong_C_enq(True);
    return m;
endmodule

(* synthesize *)
module mkDelayTLB(DelayTLB) provisos(
    NumAlias#(mshrSz, TLBMaxReqNum),
    Alias#(mshrIdxT, Bit#(TLog#(mshrSz))),
    NumAlias#(maxDelay, TLBMaxDelay),
    Alias#(delayT, Bit#(TLog#(maxDelay)))
);
    // ordering: doResp < procReq
    
    // EHR ports for valid & specBits
    Integer respPort = 0;
    Integer reqPort = 1;
    Integer wrongSpecPort = 0;
    Integer correctSpecPort = 2;

    Vector#(mshrSz, Ehr#(2, Bool)) valid <- replicateM(mkEhr(False));
    Vector#(mshrSz, Reg#(DelayTLBReq)) req <- replicateM(mkRegU);
    Vector#(mshrSz, Ehr#(3, SpecBits)) specBits <- replicateM(mkEhr(?));
    Vector#(mshrSz, SatDownCounter#(delayT)) delay <- replicateM(mkSatDownCounter(0));

    Randomize#(delayT) randDelay <- mkGenericRandomizer;
    Reg#(Bool) inited <- mkReg(False);

    TLBRespQ respQ <- mkTLBRespQ;

    // make incorrectSpeculation conflict with all others except doResp and correctSpec
    RWire#(void) wrongSpec_req_conflict <- mkRWire;
    RWire#(void) wrongSpec_resp_conflict <- mkRWire;

    rule doInitRand(!inited);
        randDelay.cntrl.init;
        inited <= True;
    endrule

    rule doResp;
        function Bool canResp(Integer i);
            return valid[i][respPort] && delay[i] == 0;
        endfunction
        Vector#(mshrSz, Integer) idxVec = genVector;
        if(findIndex(canResp, idxVec) matches tagged Valid .idx) begin
            valid[idx][respPort] <= False;
            respQ.enq(ToSpecFifo {
                data: req[idx],
                spec_bits: specBits[idx][respPort]
            });
        end
        else begin
            when(False, noAction);
        end
        // make conflict with incorrect spec 
        wrongSpec_resp_conflict.wset(?);
    endrule

    (* fire_when_enabled, no_implicit_conditions *)
    rule doDecrDelay;
        function Action decrDelay(Integer i) = delay[i].dec;
        Vector#(mshrSz, Integer) idxVec = genVector;
        joinActions(map(decrDelay, idxVec));
    endrule

    // approximate the req entry using stale valid bits
    Wire#(Maybe#(mshrIdxT)) reqIdx <- mkBypassWire;
    (* fire_when_enabled, no_implicit_conditions *)
    rule findReqIdx;
        if(findIndex( \== (False) , readVEhr(0, valid) ) matches tagged Valid .idx) begin
            reqIdx <= Valid (pack(idx));
        end
        else begin
            reqIdx <= Invalid;
        end
    endrule

    method Action procReq(DelayTLBReq r, SpecBits sb) if(inited &&& reqIdx matches tagged Valid .idx);
        valid[idx][reqPort] <= True;
        req[idx] <= r;
        specBits[idx][reqPort] <= sb;
        let lat <- randDelay.next;
        delay[idx] <= lat;
        // make conflict with incorrect spec
        wrongSpec_req_conflict.wset(?);
    endmethod

    method respVal = respQ.first.data;
    method deqResp = respQ.deq;

    interface SpeculationUpdate specUpdate;
        method Action correctSpeculation(SpecBits mask);
            // clear spec bits for all entries
            function Action correctSpec(Integer i);
            action
                SpecBits sb = specBits[i][correctSpecPort];
                specBits[i][correctSpecPort] <= sb & mask;
            endaction
            endfunction
            Vector#(mshrSz, Integer) idxVec = genVector;
            joinActions(map(correctSpec, idxVec));
            // clear spec bits for respQ
            respQ.specUpdate.correctSpeculation(mask);
        endmethod

        method Action incorrectSpeculation(SpecTag specTag);
            // clear entries
            function Action incorrectSpec(Integer i);
            action
                SpecBits sb = specBits[i][wrongSpecPort];
                if(sb[specTag] == 1) begin
                    valid[i][wrongSpecPort] <= False;
                end
            endaction
            endfunction
            Vector#(mshrSz, Integer) idxVec = genVector;
            joinActions(map(incorrectSpec, idxVec));
            // clear respQ
            respQ.specUpdate.incorrectSpeculation(specTag);
            // make conflict with others
            wrongSpec_req_conflict.wset(?);
            wrongSpec_resp_conflict.wset(?);
        endmethod
    endinterface
endmodule
