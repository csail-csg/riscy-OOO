
// Copyright (c) 2019 Massachusetts Institute of Technology
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

import Vector::*;
import ConfigReg::*;
import Assert::*;
import Ehr::*;
import Types::*;
import ProcTypes::*;
import HasSpecBits::*;

// Reservation station used in in-order core to hold instructions after
// renaming. This is simply a FIFO, and instructions are ordered in program
// order. Outside rules only inspect the head of the FIFO, i.e., there is no
// need to receive "wake up" from outside. Killing is also simplified: we can
// just move the enqP.

typedef struct {
    dataT data; // payload for this inst
    PhyRegs regs;
    InstTag tag;
    // speculation
    SpecBits spec_bits;
    Maybe#(SpecTag) spec_tag;
} ToInorderRS#(type dataT) deriving(Bits, Eq, FShow);

interface InorderRS#(numeric type size, type dataT);
    method Action enq(ToInorderRS#(dataT) x);
    method Bool canEnq;

    method ToInorderRS#(dataT) first;
    method Action deq;

    // For count-based scheduling when there are multiple reservation stations
    // for the same inst type. This method only takes effect when module
    // parameter countValid is true.
    method Bit#(TLog#(TAdd#(size, 1))) approximateCount;

    // performance: count full cycles
    method Bool isFull_ehrPort0;

    interface SpeculationUpdate specUpdate;
endinterface

// schedule:
// deq < enq < correctSpec
// (deq, enq) C wrongSpec

module mkInorderRS#(Bool lazyEnq, Bool countValid)(
    InorderRS#(size, dataT)
) provisos (
    Alias#(idxT, Bit#(TLog#(size))),
    Alias#(virIdxT, Bit#(TLog#(TMul#(2, size)))), // virtual index
    Alias#(countT, Bit#(TLog#(TAdd#(size, 1)))),
    Log#(TAdd#(1, size), TLog#(TAdd#(size, 1))),
    Bits#(dataT, a__),
    Add#(1, b__, size),
    Add#(c__, TLog#(size), TLog#(TMul#(2, size)))
);
    Vector#(size, Ehr#(2, Bool))         valid    <- replicateM(mkEhr(False));
    Vector#(size, Reg#(dataT))           data     <- replicateM(mkRegU);
    Vector#(size, Reg#(PhyRegs))         regs     <- replicateM(mkRegU);
    Vector#(size, Reg#(InstTag))         instTag  <- replicateM(mkRegU);
    Vector#(size, Reg#(Maybe#(SpecTag))) specTag  <- replicateM(mkRegU);
    Vector#(size, Ehr#(2, SpecBits))     specBits <- replicateM(mkEhr(?));

    let valid_deq       = getVEhrPort(valid, 0);
    let valid_wrongSpec = getVEhrPort(valid, 0); // write
    let valid_enq       = getVEhrPort(valid, 1); // write

    let sb_deq         = getVEhrPort(specBits, 0);
    let sb_wrongSpec   = getVEhrPort(specBits, 0);
    let sb_enq         = getVEhrPort(specBits, 0); // write
    let sb_correctSpec = getVEhrPort(specBits, 1); // write

    // create conflicts
    RWire#(void) wrongSpec_enq_conflict <- mkRWire;
    RWire#(void) wrongSpec_deq_conflict <- mkRWire;

    Reg#(idxT) enqP <- mkReg(0);
    Reg#(idxT) deqP <- mkReg(0);

    function idxT getNextPtr(idxT i);
        return i == fromInteger(valueOf(size) - 1) ? 0 : i + 1;
    endfunction

    // We use virtual index to determine which is the oldest entry being
    // killed.  Since enqP is not changed during all the cycle, we map index to
    // virtual index using enqP as pivot.
    // The mapping is as follow:
    // - valid entry i --> i < enqP ? i + QSize : i
    // XXX This mapping is only for comparing valid entries (e.g., it may not
    // work properly for enqP), so we must check entry valid before using
    // virtual index.
    function virIdxT getVirIdx(idxT i);
        return i < enqP ? zeroExtend(i) + fromInteger(valueof(size))
                        : zeroExtend(i);
    endfunction
    // Virtual index
    Vector#(size, virIdxT) virIdxes = map(getVirIdx, genWith(fromInteger));

    // find the oldest entry that satisfies a constraint (i.e. smallest virtual
    // index)
    function Maybe#(idxT) findOldest(Vector#(size, Bool) pred);
        function idxT getOlder(idxT a, idxT b);
            if(!pred[a]) begin
                return b;
            end
            else if(!pred[b]) begin
                return a;
            end
            else begin
                return virIdxes[a] < virIdxes[b] ? a : b;
            end
        endfunction
        Vector#(size, idxT) idxVec = genWith(fromInteger);
        idxT i  = fold(getOlder, idxVec);
        return pred[i] ? Valid (i) : Invalid;
    endfunction

    // approximate count of valid entries
    Reg#(countT) validEntryCount <- mkConfigReg(0);

    if(countValid) begin
        (* fire_when_enabled, no_implicit_conditions *)
        rule countValidEntries;
            validEntryCount <= pack(countElem(True, readVEhr(0, valid)));
        endrule
    end

    // can enq signal for enq port
    Bool can_enq;
    if(lazyEnq) begin
        Wire#(Bool) can_enq_wire <- mkBypassWire;
        (* fire_when_enabled, no_implicit_conditions *)
        rule setCanEnq;
            can_enq_wire <= !valid[enqP][0];
        endrule
        can_enq = can_enq_wire;
    end
    else begin
        can_enq = !valid_enq[enqP];
    end

    // can deq sigal for deq port
    Bool can_deq = valid_deq[deqP];

`ifdef BSIM
    // Sanity check in simulation. All valid entry are consective within deqP
    // and enqP, outsiders are invalid entries
    (* fire_when_enabled, no_implicit_conditions *)
    rule checkValid;
        if(all(\== (False),  readVEhr(0, valid))) begin
            doAssert(enqP == deqP, "empty queue have enqP = deqP");
        end
        else begin
            // not empty queue, check valid entries with [deqP, enqP)
            function Bool in_range(idxT i);
                if(deqP < enqP) begin
                    return deqP <= i && i < enqP;
                end
                else begin
                    return deqP <= i || i < enqP;
                end
            endfunction
            for(Integer i = 0; i < valueof(size); i = i+1) begin
                doAssert(in_range(fromInteger(i)) == valid[i][0],
                         "valid entries must be within [deqP, enqP)");
            end
        end
    endrule
`endif

    method Action enq(ToInorderRS#(dataT) x) if(can_enq);
        doAssert(!valid_enq[enqP], "enqP entry must be invalid");
        // set contents
        data[enqP] <= x.data;
        regs[enqP] <= x.regs;
        instTag[enqP] <= x.tag;
        specTag[enqP] <= x.spec_tag;
        sb_enq[enqP] <= x.spec_bits;
        // set valid, move ptr
        valid_enq[enqP] <= True;
        enqP <= getNextPtr(enqP);
        // make conflict with wrong spec
        wrongSpec_enq_conflict.wset(?);
    endmethod

    method Bool canEnq = can_enq;

    method ToInorderRS#(dataT) first if(can_deq);
        return ToInorderRS {
            data: data[deqP],
            regs: regs[deqP],
            tag: instTag[deqP],
            spec_bits: sb_deq[deqP],
            spec_tag: specTag[deqP]
        };
    endmethod

    method Action deq if(can_deq);
        valid_deq[deqP] <= False;
        deqP <= getNextPtr(deqP);
        // make conflict with wrong spec
        wrongSpec_deq_conflict.wset(?);
    endmethod

    method countT approximateCount;
        return validEntryCount;
    endmethod

    method Bool isFull_ehrPort0;
        return readVEhr(0, valid) == replicate(True);
    endmethod

    interface SpeculationUpdate specUpdate;
        method Action correctSpeculation(SpecBits mask);
            function Action correctSpec(idxT i);
            action
                sb_correctSpec[i] <= sb_correctSpec[i] & mask;
            endaction
            endfunction
            Vector#(size, idxT) idxVec = genWith(fromInteger);
            joinActions(map(correctSpec, idxVec));
        endmethod

        method Action incorrectSpeculation(Bool killAll, SpecTag killSpecTag);
            // clear wrong path entries
            Vector#(size, idxT) idxVec = genWith(fromInteger);
            function Bool isNeedKill(idxT i);
                return killAll || sb_wrongSpec[i][killSpecTag] == 1;
            endfunction
            Vector#(size, Bool) needKill = map(isNeedKill, idxVec);
            function Action kill(idxT i);
            action
                if(needKill[i]) begin
                    valid_wrongSpec[i] <= False;
                end
            endaction
            endfunction
            joinActions(map(kill, idxVec));

            // change enqP: make valid entries always consecutive: new enqP is
            // the oldest **VALID** entry that gets killed. If such entry does
            // not exists, then enqP remains the same.
            function Bool isValidKilled(idxT i);
                return valid_wrongSpec[i] && needKill[i];
            endfunction
            Vector#(size, Bool) killedValid = map(isValidKilled, idxVec);
            idxT new_enqP = enqP;
            if(findOldest(killedValid) matches tagged Valid .idx) begin
                new_enqP = idx;
            end
            enqP <= new_enqP;

            // conflict with enq, deq
            wrongSpec_enq_conflict.wset(?);
            wrongSpec_deq_conflict.wset(?);
        endmethod
    endinterface
endmodule
