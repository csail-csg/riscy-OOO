import Types::*;
import ProcTypes::*;
import TestTypes::*;
import Vector::*;
import HasSpecBits::*;
import SpecFifo::*;
import Randomizable::*;
import Ehr::*;

// simply model the delay in an OOO TLB, it doesn't really do any translation.
// We need to kill wrong-path req

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
    // ordering: doResp < dec delay < procReq
    
    Vector#(mshrSz, Ehr#(2, Bool)) valid <- replicateM(mkEhr(False));
    let valid_resp      = getVEhrPort(valid, 0); // write
    let valid_wrongSpec = getVEhrPort(valid, 0); // write
    let valid_req       = getVEhrPort(valid, 1); // write
    Vector#(mshrSz, Reg#(DelayTLBReq)) req <- replicateM(mkRegU);
    Vector#(mshrSz, Ehr#(2, SpecBits)) specBits <- replicateM(mkEhr(?));
    let specBits_resp        = getVEhrPort(specBits, 0);
    let specBits_wrongSpec   = getVEhrPort(specBits, 0);
    let specBits_req         = getVEhrPort(specBits, 0); // write
    let specBits_correctSpec = getVEhrPort(specBits, 1); // write
    Vector#(mshrSz, Ehr#(2, delayT)) delay <- replicateM(mkEhr(0));
    let delay_resp = getVEhrPort(delay, 0);
    let delay_dec  = getVEhrPort(delay, 0); // write
    let delay_req  = getVEhrPort(delay, 1); // write

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
        function Bool canResp(mshrIdxT i);
            return valid_resp[i] && delay_resp[i] == 0;
        endfunction
        Vector#(mshrSz, mshrIdxT) idxVec = genWith(fromInteger);
        if(find(canResp, idxVec) matches tagged Valid .idx) begin
            valid_resp[idx] <= False;
            respQ.enq(ToSpecFifo {
                data: req[idx],
                spec_bits: specBits_resp[idx]
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
        function Action decrDelay(Integer i) = (action
            if(delay_dec[i] > 0) begin
                delay_dec[i] <= delay_dec[i] - 1;
            end
        endaction);
        Vector#(mshrSz, Integer) idxVec = genVector;
        joinActions(map(decrDelay, idxVec));
    endrule

    method Action procReq(DelayTLBReq r, SpecBits sb) if(
        inited &&&
        findIndex(\== (False), readVReg(valid_req)) matches tagged Valid .idx
    );
        valid_req[idx] <= True;
        req[idx] <= r;
        specBits_req[idx] <= sb;
        let lat <- randDelay.next;
        delay_req[idx] <= lat;
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
                SpecBits sb = specBits_correctSpec[i];
                specBits_correctSpec[i] <= sb & mask;
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
                SpecBits sb = specBits_wrongSpec[i];
                if(sb[specTag] == 1) begin
                    valid_wrongSpec[i] <= False;
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
