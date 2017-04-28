
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
// ReorderBuffer
import Types::*;
import ProcTypes::*;
import HasSpecBits::*;
import Vector::*;
import Assert::*;
import Ehr::*;
import RevertingVirtualReg::*;

// right after execution, full_result has more up-to-date data (e.g. ppc of mispredicted branch)
// some parts of full_result are for verification
// but some are truly used for execution

// ppc is only used by iType = BR/J/JR
// csrData is only used by iType = Csr
// vaddr is only used by mem inst in page fault
typedef union tagged {
    Addr PPC; // at default store ppc
    Addr VAddr; // for mem inst, store vaddr
    Data CSRData; // for Csr inst, store csr_data
} PPCVAddrCSRData deriving(Bits, Eq, FShow);

typedef struct {
    Addr                pc;
    IType               iType;
    Maybe#(CSR)         csr;
    //Maybe#(RenamingTag) rename_tag; // for fast killing of renaming table
    Bool                claimed_phy_reg; // whether we need to commmit renaming
    Maybe#(Trap)        trap;
    PPCVAddrCSRData     ppc_vaddr_csrData;
    Bit#(5)             fflags;
    Bool                will_dirty_fpu_state; // True means 2'b11 will be written to FS
    RobInstState        rob_inst_state; // was executed

    // speculation
    SpecBits            spec_bits;
    Maybe#(SpecTag)     spec_tag;

    // for verification only
`ifdef VERIFICATION_PACKETS
    Instruction         inst;
    Maybe#(ArchRIndx)   arch_reg_dst;
    Data                result_data; // data written into dst reg
`endif
} ToReorderBuffer deriving(Bits, Eq, FShow);

typedef enum {
    InRStation,
    InLdStQ,
    Executed
} RobInstState deriving (Bits, Eq, FShow);

interface Row_setExecuted_doFinishAlu;
    method Action set(Data res, Maybe#(Data) csrData, ControlFlow cf, RobInstState new_state);
endinterface

interface ReorderBufferRowEhr#(numeric type aluExeNum);
    method Action write_enq(ToReorderBuffer x);
    method ToReorderBuffer read_deq;
    // record Ld/Amo/Lr/Sc result just for verify packets
    method Action setExecuted_deqLSQ(Data res, RobInstState new_state);
    // doFinishXXX rules set ROB state (future may have vector of doFinishAlu ifc)
    interface Vector#(aluExeNum, Row_setExecuted_doFinishAlu) setExecuted_doFinishAlu;
    method Action setExecuted_doFinishFpuMulDiv(Data res, Bit#(5) fflags, RobInstState new_state);
    method Action setExecuted_doFinishMem(Data data, Addr vaddr, Maybe#(Exception) cause, RobInstState new_state);
    // for Ld in doFinishMem without exception, set this Ld to depend on itself's spec tag
    // this won't be called simultaneously with correct or wrong specution in one rule
    // (spec tag of this Ld is kept)
    method Action setLdSpecBit(SpecTag ldSpecTag);
    // get original PC/PPC before execution, EHR port 0 will suffice
    method Addr getOrigPC;
    method Addr getOrigPredPC;
    // get renaming tag for fast kill in renaming table
    //method Maybe#(RenamingTag) getRenameTag;
    // speculation
    method Bool dependsOn_wrongSpec(SpecTag tag);
    method Action correctSpeculation(SpecBits mask);
endinterface

module mkReorderBufferRowEhr(ReorderBufferRowEhr#(aluExeNum)) provisos(Add#(1, a__, aluExeNum));
    Integer trap_deq_port = 0;
    Integer trap_finishMem_port = 0; // write trap
    Integer trap_enq_port = 1; // write trap

    Integer pvc_deq_port = 0;
    function Integer pvc_finishAlu_port(Integer i) = i; // write ppc_vaddr_csrData
    Integer pvc_finishMem_port = valueof(aluExeNum); // write ppc_vaddr_csrData
    Integer pvc_enq_port = 1 + valueof(aluExeNum); // write ppc_vaddr_csrData

    Integer fflags_deq_port = 0;
    Integer fflags_finishFpuMulDiv_port = 0; // write fflags
    Integer fflags_enq_port = 1; // write fflags

    Integer state_deq_port = 0;
    Integer state_deqLSQ_port = 0; // write state
    function Integer state_finishAlu_port(Integer i) = 1 + i; // write state
    Integer state_finishFpuMulDiv_port = 1 + valueof(aluExeNum); // write state
    Integer state_finishMem_port = 2 + valueof(aluExeNum); // write state
    Integer state_enq_port = 3 + valueof(aluExeNum); // write state

    Integer sb_deq_port = 0;
    Integer sb_wrongSpec_port = 0;
    Integer sb_setLd_port = 0; // write spec_bits
    Integer sb_enq_port = 1; // write spec_bits
    Integer sb_correctSpec_port = 2; // write spec_bits

`ifdef VERIFICATION_PACKETS
    Integer result_deq_port = 0;
    Integer result_deqLSQ_port = 0; // write result_data
    function Integer result_finishAlu_port(Integer i) = 1 + i; // write result_data
    Integer result_finishFpuMulDiv_port = 1 + valueof(aluExeNum); // write result_data
    Integer result_finishMem_port = 2 + valueof(aluExeNum); // write result_data
    Integer result_enq_port = 3 + valueof(aluExeNum); // write result_data
`endif

    Reg#(Addr)                                 pc                   <- mkRegU;
    Reg#(IType)                                iType                <- mkRegU;
    Reg#(Maybe#(CSR))                          csr                  <- mkRegU;
    //Reg#(Maybe#(RenamingTag))                  rename_tag           <- mkRegU;
    Reg#(Bool)                                 claimed_phy_reg      <- mkRegU;
    Ehr#(2, Maybe#(Trap))                      trap                 <- mkEhr(?);
    Ehr#(TAdd#(2, aluExeNum), PPCVAddrCSRData) ppc_vaddr_csrData    <- mkEhr(?);
    Ehr#(2, Bit#(5))                           fflags               <- mkEhr(?);
    Reg#(Bool)                                 will_dirty_fpu_state <- mkRegU;
    Ehr#(TAdd#(4, aluExeNum), RobInstState)    rob_inst_state       <- mkEhr(?);
    Reg#(Maybe#(SpecTag))                      spec_tag             <- mkRegU;
    Ehr#(3, SpecBits)                          spec_bits            <- mkEhr(?);
`ifdef VERIFICATION_PACKETS
    Reg#(Instruction)                          inst                 <- mkRegU;
    Reg#(Maybe#(ArchRIndx))                    arch_reg_dst         <- mkRegU;
    Ehr#(TAdd#(4, aluExeNum), Data)            result_data          <- mkEhr(?);
`endif

    // wires to get stale (EHR port 0) values of PPC
    Wire#(Addr) predPcWire <- mkBypassWire;
    (* fire_when_enabled, no_implicit_conditions *)
    rule setPcWires;
        predPcWire <= ppc_vaddr_csrData[0] matches tagged PPC .a ? a : 0;
    endrule

    Vector#(aluExeNum, Row_setExecuted_doFinishAlu) aluSetExe;
    for(Integer i = 0; i < valueof(aluExeNum); i = i+1) begin
        aluSetExe[i] = (interface Row_setExecuted_doFinishAlu;
            method Action set(Data res, Maybe#(Data) csrData, ControlFlow cf, RobInstState new_state);
                // always update ROB state
                rob_inst_state[state_finishAlu_port(i)] <= new_state; 
                // update PPC or csrData (vaddr is always useless for ALU results)
                if(csrData matches tagged Valid .d) begin
                    ppc_vaddr_csrData[pvc_finishAlu_port(i)] <= CSRData (d);
                end
                else begin
                    ppc_vaddr_csrData[pvc_finishAlu_port(i)] <= PPC (cf.nextPc);
                end
                doAssert(isValid(csr) == isValid(csrData), "csr valid should match");
                // update result for verification
`ifdef VERIFICATION_PACKETS
                result_data[result_finishAlu_port(i)] <= res;
`endif
            endmethod
        endinterface);
    end

    method Addr getOrigPC = pc;
    method Addr getOrigPredPC = predPcWire;
    //method Maybe#(RenamingTag) getRenameTag = rename_tag;

    interface setExecuted_doFinishAlu = aluSetExe;

    method Action setExecuted_doFinishFpuMulDiv(Data res, Bit#(5) new_fflags, RobInstState new_state);
        // always update ROB state
        rob_inst_state[state_finishFpuMulDiv_port] <= new_state; 
        // update fflags
        fflags[fflags_finishFpuMulDiv_port] <= new_fflags;
        // update result for verification
`ifdef VERIFICATION_PACKETS
        result_data[result_finishFpuMulDiv_port] <= res;
`endif
    endmethod

    method Action setExecuted_doFinishMem(Data data, Addr vaddr, Maybe#(Exception) cause, RobInstState new_state);
        // always update ROB state
        rob_inst_state[state_finishMem_port] <= new_state; 
        // update trap
        if(cause matches tagged Valid .e &&& !isValid(trap[trap_finishMem_port])) begin
            trap[trap_finishMem_port] <= Valid (Exception (e));
        end
        // update VAddr
        ppc_vaddr_csrData[pvc_finishMem_port] <= VAddr (vaddr);
        // update result for verification
`ifdef VERIFICATION_PACKETS
        result_data[result_finishMem_port] <= data;
`endif
    endmethod

    method Action write_enq(ToReorderBuffer x);
        pc <= x.pc;
        iType <= x.iType;
        csr <= x.csr;
        //rename_tag <= x.rename_tag;
        claimed_phy_reg <= x.claimed_phy_reg;
        trap[trap_enq_port] <= x.trap;
        ppc_vaddr_csrData[pvc_enq_port] <= x.ppc_vaddr_csrData;
        fflags[fflags_enq_port] <= x.fflags;
        will_dirty_fpu_state <= x.will_dirty_fpu_state;
        rob_inst_state[state_enq_port] <= x.rob_inst_state;
        spec_bits[sb_enq_port] <= x.spec_bits;
        spec_tag <= x.spec_tag;
`ifdef VERIFICATION_PACKETS
        inst <= x.inst;
        arch_reg_dst <= x.arch_reg_dst;
        result_data[result_enq_port] <= x.result_data;
`endif
    endmethod

    method ToReorderBuffer read_deq;
        return ToReorderBuffer {
            pc: pc,
            iType: iType,
            csr: csr,
            //rename_tag: rename_tag,
            claimed_phy_reg: claimed_phy_reg,
            trap: trap[trap_deq_port],
            ppc_vaddr_csrData: ppc_vaddr_csrData[pvc_deq_port],
            fflags: fflags[fflags_deq_port],
            will_dirty_fpu_state: will_dirty_fpu_state,
            rob_inst_state: rob_inst_state[state_deq_port],
            spec_bits: spec_bits[sb_deq_port],
            spec_tag: spec_tag
`ifdef VERIFICATION_PACKETS
            , inst: inst
            , arch_reg_dst: arch_reg_dst
            , result_data: result_data[result_deq_port]
`endif
        };
    endmethod

    method Action setExecuted_deqLSQ(Data res, RobInstState new_state);
`ifdef VERIFICATION_PACKETS
        result_data[result_deqLSQ_port] <= res;
`endif
        rob_inst_state[state_deqLSQ_port] <= new_state;
    endmethod

    method Action setLdSpecBit(SpecTag ldSpecTag);
        spec_bits[sb_setLd_port][ldSpecTag] <= 1;
    endmethod

    method Bool dependsOn_wrongSpec(SpecTag tag);
        return spec_bits[sb_wrongSpec_port][tag] == 1;
    endmethod

    method Action correctSpeculation(SpecBits mask);
        SpecBits sb = spec_bits[sb_correctSpec_port];
        spec_bits[sb_correctSpec_port] <= sb & mask;
    endmethod
endmodule

interface ROB_SpeculationUpdate;
    // when killing wrong path inst, we directly move enqP to inst_tag + 1
    // assumption is that any entry within (inst_tag, enqP) will be killed
    // (i.e. any such entry has spec_bits[spec_tag] == 1, so valid bit is reset)
    // notice that inst_tag itself may be killed! (e.g. a Ld killed by older Ld/St)
    // also note that inst_tag itself may be already dequeued just in this cycle
    method Action incorrectSpeculation(SpecTag spec_tag, InstTag inst_tag);
    method Action correctSpeculation(SpecBits mask);
endinterface

`ifndef SUP_ROB
///////////////////////////
// single scalar version //
//////////////////////////

error("Single scalar ROB is not maintained");

interface ReorderBufferEhr;
    method Action enq(ToReorderBuffer x);
    method InstTag getEnqInstTag;
    method Bool isEmpty; // empty signal for enq port (for FENCE/System inst etc.)

    method Action deq;
    method InstTag getDeqInstTag;
    method ToReorderBuffer deq_data;

    // record Ld/Amo/Lr/Sc result just for verify packets
    method Action setExecuted_deqLSQ(InstTag x, Data res, RobInstState new_state);
    // doFinishXXX rules set ROB state
    method Action setExecuted_doFinishAlu(InstTag x, Data res, Maybe#(Data) csrData, ControlFlow cf, RobInstState new_state);
    method Action setExecuted_doFinishFpuMulDiv(InstTag x, Data res, Bit#(5) fflags, RobInstState new_state);
    method Action setExecuted_doFinishMem(InstTag x, Data data, Addr vaddr, Maybe#(Exception) cause, RobInstState new_state);
    // for Ld in doFinishMem without exception, set this Ld to depend on itself's spec tag
    // this won't be called simultaneously with correct or wrong specution in one rule
    // (spec tag of this Ld is kept)
    method Action setLdSpecBit(InstTag x, SpecTag ldSpecTag);

    // get original PC/PPC before execution, EHR port 0 will suffice
    method Addr getOrigPC(InstTag x);
    method Addr getOrigPredPC(InstTag x);
    // get renaming tag for fast kill in renaming table
    //method Maybe#(RenamingTag) getRenameTag(InstTag x);

    method Bool isEmpty_ehrPort0;
    method Bool isFull_ehrPort0;

    interface ROB_SpeculationUpdate specUpdate;
endinterface

module mkReorderBufferEhr#(Bool lazyEnq)(ReorderBufferEhr);
    // doCommit rule: deq, deq_data
    Integer deq_port = 0;

    // doFinishXXX, doDeqLSQ_XXX: setExecute_XXX, correctSpeculation
    // these are handled in mkReorderBufferRowEhr

    // doRenaming rule: enq, getEnqInstTag, isEmpty
    Integer enq_port = 1;

    // wrong speculation: overwrite deq in the same doCommit rule
    // (this will be set to conflict with enq, so can use EHR port 1)
    Integer wrongSpec_port = 1;
    // make wrong speculation conflict with enq
    RWire#(void) wrongSpec_enq_conflict <- mkRWire;

    Vector#(NumInstTags, ReorderBufferRowEhr) row <- replicateM(mkReorderBufferRowEhr);
    Vector#(NumInstTags, Ehr#(2, Bool)) valid <- replicateM(mkEhr(False));
    Ehr#(2, InstTag) deqP <- mkEhr(0);
    Reg#(InstTag) enqP <- mkReg(0);

    function InstTag getNextPtr(InstTag p);
        return p == fromInteger(valueOf(NumInstTags)-1) ? 0 : p + 1;
    endfunction

`ifdef BSIM
    // sanity check in simulation
    // all valid entry are within [deqP, enqP), outsiders are invalid entries
    (* fire_when_enabled, no_implicit_conditions *)
    rule sanityCheck;
        Bool empty = all( \== (False), readVEhr(0, valid) );
        function Bool in_range(InstTag i);
            // i is within [deqP, enqP)
            if(empty) begin
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
        for(Integer i = 0; i < valueof(NumInstTags); i = i+1) begin
            doAssert(in_range(fromInteger(i)) == valid[i][0],
                "entries inside [deqP, enqP) should be valid, otherwise invalid"
            );
        end
    endrule
`endif

    Bool empty_for_enq = ?; // enq port empty signal (enqP == deqP && !valid[enqP])
    Bool can_enq = ?; // when enq slot is not valid
    if(lazyEnq) begin
        Wire#(Bool) empty_for_enq_wire <- mkBypassWire;
        Wire#(Bool) can_enq_wire <- mkBypassWire;
        (* fire_when_enabled, no_implicit_conditions *)
        rule setEnqWires;
            empty_for_enq_wire <= !valid[enqP][0] && enqP == deqP[0];
            can_enq_wire <= !valid[enqP][0];
        endrule
        empty_for_enq = empty_for_enq_wire;
        can_enq = can_enq_wire;
    end
    else begin
        empty_for_enq = !valid[enqP][enq_port] && enqP == deqP[enq_port];
        can_enq = !valid[enqP][enq_port];
    end

    method Action enq(ToReorderBuffer x) if(can_enq);
        enqP <= getNextPtr(enqP);
        row[enqP].write_enq(x);
        valid[enqP][enq_port] <= True;
        // make it conflict with wrong speculation
        wrongSpec_enq_conflict.wset(?);
    endmethod

    method Bool isEmpty;
        return empty_for_enq;
    endmethod

    method InstTag getEnqInstTag;
        return enqP;
    endmethod

    method Bool isEmpty_ehrPort0;
        return !valid[enqP][0] && enqP == deqP[0];
    endmethod

    method Bool isFull_ehrPort0;
        return valid[enqP][0] && enqP == deqP[0];
    endmethod

    method Action deq if (valid[deqP[deq_port]][deq_port]);
        deqP[deq_port] <= getNextPtr(deqP[deq_port]);
        valid[deqP[deq_port]][deq_port] <= False;
    endmethod

    method InstTag getDeqInstTag if (valid[deqP[deq_port]][deq_port]);
        return deqP[deq_port];
    endmethod

    method ToReorderBuffer deq_data if (valid[deqP[deq_port]][deq_port]);
        return row[deqP[deq_port]].read_deq;
    endmethod

    method Action setExecuted_deqLSQ(InstTag x, Data res, RobInstState new_state);
        row[x].setExecuted_deqLSQ(res, new_state);
    endmethod

    method Action setExecuted_doFinishAlu(InstTag x, Data res, Maybe#(Data) csrData, ControlFlow cf, RobInstState new_state);
        row[x].setExecuted_doFinishAlu(res, csrData, cf, new_state);
    endmethod

    method Action setExecuted_doFinishFpuMulDiv(InstTag x, Data res, Bit#(5) fflags, RobInstState new_state);
        row[x].setExecuted_doFinishFpuMulDiv(res, fflags, new_state);
    endmethod

    method Action setExecuted_doFinishMem(InstTag x, Data data, Addr vaddr, Maybe#(Exception) cause, RobInstState new_state);
        row[x].setExecuted_doFinishMem(data, vaddr, cause, new_state);
    endmethod

    method Action setLdSpecBit(InstTag x, SpecTag ldSpecTag);
        row[x].setLdSpecBit(ldSpecTag);
    endmethod

    method Addr getOrigPC(InstTag x) = row[x].getOrigPC;
    method Addr getOrigPredPC(InstTag x) = row[x].getOrigPredPC;
    //method Maybe#(RenamingTag) getRenameTag(InstTag x) = row[x].getRenameTag;

    interface ROB_SpeculationUpdate specUpdate;
        method Action correctSpeculation(SpecBits mask);
            for (Integer i = 0 ; i < valueOf(NumInstTags) ; i = i+1) begin
                row[i].correctSpeculation(mask);
            end
        endmethod

        method Action incorrectSpeculation(SpecTag x, InstTag inst_tag);
            // only update valid, no need to change spec bits
            for (Integer i = 0 ; i < valueOf(NumInstTags) ; i = i+1) begin
                if (row[i].dependsOn_wrongSpec(x)) begin
                    valid[i][wrongSpec_port] <= False;
                end
            end
            // move enqP to be right after OR just the inst that initiates the kill
            if(valid[inst_tag][wrongSpec_port] && row[inst_tag].dependsOn_wrongSpec(x)) begin
                // the kill-initiating inst also kills itself (e.g. a Ld)
                enqP <= inst_tag;
            end
            else begin
                // the kill-initiating inst does not kill itself
                enqP <= getNextPtr(inst_tag);
            end
            // make it conflict with enq
            wrongSpec_enq_conflict.wset(?);

`ifdef BSIM
            // sanity check in simulation
            function Bool getDepOn(Integer i) = row[i].dependsOn_wrongSpec(x);
            Vector#(NumInstTags, Bool) depVec = map(getDepOn, genVector);
            Vector#(NumInstTags, Bool) validVec = readVEhr(wrongSpec_port, valid);
            $display("[ROB incorrectSpec] ",
                fshow(x), " ; ",
                fshow(inst_tag),
                fshow(deqP[wrongSpec_port]), " ; ",
                fshow(enqP), " ; ",
                fshow(validVec), " ; ",
                fshow(depVec)
            );
            // all valid entry are within [deqP, enqP), outsiders are invalid entries
            Bool empty = all( \== (False), validVec );
            function Bool in_range(InstTag i);
                // i is within [deqP, enqP)
                if(empty) begin
                    return False;
                end
                else begin
                    if(deqP[wrongSpec_port] < enqP) begin
                        return deqP[wrongSpec_port] <= i && i < enqP;
                    end
                    else begin
                        return deqP[wrongSpec_port] <= i || i < enqP;
                    end
                end
            endfunction
            for(Integer i = 0; i < valueof(NumInstTags); i = i+1) begin
                doAssert(in_range(fromInteger(i)) == valid[i][wrongSpec_port],
                    "entries inside [deqP, enqP) should be valid, otherwise invalid"
                );
            end
            // inst_tag may be just dequeued
            if(!valid[inst_tag][wrongSpec_port]) begin
                doAssert(getNextPtr(inst_tag) == deqP[wrongSpec_port],
                    "if the kill-initiating entry is invalid, it must be just dequeued"
                );
            end
            // valid entries within (inst_tag, enqP) or [inst_tag, enqP) are killed, outsides are not
            Bool kill_itself = valid[inst_tag][wrongSpec_port] && row[inst_tag].dependsOn_wrongSpec(x);
            function Bool in_kill_range(InstTag i);
                // when inst_tag == enqP, it must be a full ROB before the kill and inst_tag is the oldest
                if(inst_tag < enqP) begin
                    return (kill_itself ? inst_tag <= i : inst_tag < i) && i < enqP;
                end
                else begin
                    return (kill_itself ? inst_tag <= i : inst_tag < i) || i < enqP;
                end
            endfunction
            for(Integer i = 0; i < valueof(NumInstTags); i = i+1) begin
                if(valid[i][wrongSpec_port]) begin
                    doAssert(in_kill_range(fromInteger(i)) == row[i].dependsOn_wrongSpec(x),
                        "valid entries inside (inst_tag, enqP) must be killed, outsiders must not"
                    );
                end
            end
`endif
        endmethod
    endinterface
endmodule

`else
////////////////////////////////////////////////////////
////////// Superscalar ehrized reorder buffer //////////
////////////////////////////////////////////////////////

interface ROB_EnqPort;
    method Bool canEnq;
    method Action enq(ToReorderBuffer x);
    method InstTag getEnqInstTag;
endinterface

interface ROB_DeqPort;
    method Bool canDeq;
    method Action deq;
    method InstTag getDeqInstTag;
    method ToReorderBuffer deq_data;
endinterface

// XXX guards of enq and deq ifc do not check that enq and deq are done consecutively
// This is the responsibility of outside world

// XXX enq and deq becomes a vector of interfaces
// We do not make them a single method with a vector as input,
// because synth boundary will make the guard conservative (i.e. ignoring input)
// thus requiring all ways to be able to enq/deq, which may cause deadlock
// However, synth boundary is needed to keep the atomicity of methods
// Having a vector of interfaces makes compiler unable to detect that different enq/deq
// interfaces are actually accessing different ways
// We have to use wires to perform the real actions in one rule so that compiler will
// not raise false conflicts between the superscalar enq/deq actions

interface ROB_setExecuted_doFinishAlu;
    method Action set(InstTag x, Data res, Maybe#(Data) csrData, ControlFlow cf, RobInstState new_state);
endinterface

interface ROB_getOrigPC;
    method Addr get(InstTag x);
endinterface

interface ROB_getOrigPredPC;
    method Addr get(InstTag x);
endinterface

interface SupReorderBuffer#(numeric type aluExeNum);
    interface Vector#(SupSize, ROB_EnqPort) enqPort;
    method Bool isEmpty; // empty signal for enq port (for FENCE/System inst etc.)

    interface Vector#(SupSize, ROB_DeqPort) deqPort;

    // record Ld/Amo/Lr/Sc result just for verify packets
    method Action setExecuted_deqLSQ(InstTag x, Data res, RobInstState new_state);
    // doFinishXXX rules set ROB state
    interface Vector#(aluExeNum, ROB_setExecuted_doFinishAlu) setExecuted_doFinishAlu;
    method Action setExecuted_doFinishFpuMulDiv(InstTag x, Data res, Bit#(5) fflags, RobInstState new_state);
    method Action setExecuted_doFinishMem(InstTag x, Data data, Addr vaddr, Maybe#(Exception) cause, RobInstState new_state);
    // for Ld in doFinishMem without exception, set this Ld to depend on itself's spec tag
    // this won't be called simultaneously with correct or wrong specution in one rule
    // (spec tag of this Ld is kept)
    method Action setLdSpecBit(InstTag x, SpecTag ldSpecTag);

    // get original PC/PPC before execution, EHR port 0 will suffice
    interface Vector#(aluExeNum, ROB_getOrigPC) getOrigPC;
    interface Vector#(aluExeNum, ROB_getOrigPredPC) getOrigPredPC;
    // get renaming tag for fast kill in renaming table
    //method Maybe#(RenamingTag) getRenameTag(InstTag x);

    // get enq time for reservation station dispatch
    method InstTime getEnqTime;

    method Bool isEmpty_ehrPort0;
    method Bool isFull_ehrPort0;

    interface ROB_SpeculationUpdate specUpdate;
endinterface

typedef struct {
    SpecTag specTag;
    InstTag killInstTag;
} ROBWrongSpecInput deriving(Bits, Eq, FShow);

typedef struct {
    SupWaySel firstEnqWay;
    Vector#(SupSize, SingleScalarPtr) enqP;
    InstTime enqTime;
} ROBWrongSpecEnqUpdate deriving(Bits, Eq, FShow);

module mkSupReorderBuffer#(
    Bool lazyEnq,
    module#(ReorderBufferRowEhr#(aluExeNum)) mkRobRow
)(SupReorderBuffer#(aluExeNum)) provisos(
    Add#(TExp#(TLog#(SupSize)), 0, SupSize), // require SupSize to be power of 2
    Add#(1, a__, aluExeNum) // at least 1 alu pipeline
);
    // doCommit rule: deq < wrongSpec (overwrite deq in doCommit) < doRenaming rule: enq
    Integer valid_deq_port = 0;
    Integer valid_wrongSpec_port = 1;
    Integer valid_enq_port = 2;

    // doFinishXXX, doDeqLSQ_XXX: setExecute_XXX, correctSpeculation
    // these are handled in mkReorderBufferRowEhr

    // wrong speculation: make wrong speculation conflict with enq
    Vector#(SupSize, RWire#(void)) wrongSpec_enq_conflict <- replicateM(mkRWire);

    // SupSize number of FIFOs
    Vector#(SupSize, Vector#(SingleScalarSize, ReorderBufferRowEhr#(aluExeNum))) row <- replicateM(replicateM(mkRobRow));
    Vector#(SupSize, Vector#(SingleScalarSize, Ehr#(3, Bool))) valid <- replicateM(replicateM(mkEhr(False)));
    Vector#(SupSize, Reg#(SingleScalarPtr)) enqP <- replicateM(mkReg(0));
`ifdef BSIM
    Vector#(SupSize, Ehr#(2, SingleScalarPtr)) deqP_ehr <- replicateM(mkEhr(0));
    Vector#(SupSize, Reg#(SingleScalarPtr)) deqP;
    for(Integer i = 0; i < valueof(SupSize); i = i+1) begin
        deqP[i] = deqP_ehr[i][0];
    end
`else
    Vector#(SupSize, Reg#(SingleScalarPtr)) deqP <- replicateM(mkReg(0));
`endif

    // enq/deq port will operate on above FIFOs in a rotating manner
    // We distinguish between enq/deq port and FIFO way
    // FIFO way is the selection of the static FIFO array
    // enq/deq port is the port selection exposed to the outside world
    // (e.g. enq/deq port 0 is for oldest inst in program order)
    // The mapping of enq/deq ports to FIFO ways changes dynamically (i.e. rotating)

    // firstEnq/DeqWay: which FIFO of row, valid, etc. that enq/deq port 0 should use
    Reg#(SupWaySel) firstEnqWay <- mkReg(0);
    Reg#(SupWaySel) firstDeqWay <- mkReg(0);

    // time of inst: enq & deq ptr as if ROB is just a FIFO of size 2^log(NumInstTag)
    Reg#(InstTime) enqTime <- mkReg(0);
    Reg#(InstTime) deqTime <- mkReg(0);

    // wires for recording actions on enq & deq ports
    Vector#(SupSize, RWire#(ToReorderBuffer)) enqEn <- replicateM(mkRWire);
    Vector#(SupSize, PulseWire) deqEn <- replicateM(mkPulseWire);
    // wire for recording action of wrongSpec
    RWire#(ROBWrongSpecInput) wrongSpecEn <- mkRWire;
    // next val for updating enq regs in case wrongSpec happens
    RWire#(ROBWrongSpecEnqUpdate) wrongSpecEnqUpdate <- mkRWire;

    // ordering regs: deq sequence before others are still mainted
    // BUT setExecuted_XXX and deq sequence before enq NEEDs explicit ordering
    Vector#(SupSize, Reg#(Bool)) deq_SB_enq <- replicateM(mkRevertingVirtualReg(True));
    Vector#(SupSize, Reg#(Bool)) setExeAlu_SB_enq <- replicateM(mkRevertingVirtualReg(True));
    Vector#(SupSize, Reg#(Bool)) setExeMem_SB_enq <- replicateM(mkRevertingVirtualReg(True));
    Vector#(SupSize, Reg#(Bool)) setExeLSQ_SB_enq <- replicateM(mkRevertingVirtualReg(True));
    Vector#(SupSize, Reg#(Bool)) setExeFpuMulDiv_SB_enq <- replicateM(mkRevertingVirtualReg(True));
    Vector#(SupSize, Reg#(Bool)) setLdSB_SB_enq <- replicateM(mkRevertingVirtualReg(True));

    function SingleScalarPtr getNextPtr(SingleScalarPtr p);
        return p == fromInteger(valueOf(SingleScalarSize)-1) ? 0 : p + 1;
    endfunction

    // convert enq/deq port -> fifo way
    function SupWaySel getEnqFifoWay(SupWaySel enqPort) = firstEnqWay + enqPort;
    function SupWaySel getDeqFifoWay(SupWaySel deqPort) = firstDeqWay + deqPort;
    // convert fifo way -> enq/deq port
    function SupWaySel getEnqPort(SupWaySel fifoWay) = fifoWay - firstEnqWay;
    function SupWaySel getDeqPort(SupWaySel fifoWay) = fifoWay - firstDeqWay;
    // XXX above 4 functions require SupSize to be power of 2

    // do deq & update firstDeqWay
    // XXX also process wrongSpec: clear valid bits & compute update for enq regs
    (* fire_when_enabled, no_implicit_conditions *)
    rule canon_deq_wrongSpec;
        // -- apply effects of deq --
        for(Integer i = 0; i < valueof(SupSize); i = i+1) begin
            // for FIFO way i (looping for FIFO way should save area than looping for deq port)
            SupWaySel deqPort = getDeqPort(fromInteger(i));
            doAssert(getDeqFifoWay(deqPort) == fromInteger(i), "deq port matches FIFO way");
            if(deqEn[deqPort]) begin
                doAssert(valid[i][deqP[i]][valid_deq_port], "deq entry must be valid");
                // move deqP & reset valid
                deqP[i] <= getNextPtr(deqP[i]);
                valid[i][deqP[i]][valid_deq_port] <= False;
            end
        end
        // update firstDeqWay: find the first deq port that is not enabled
        Vector#(SupSize, SupWaySel) idxVec = genWith(fromInteger);
        function Bool notDeq(SupWaySel i);
            return !deqEn[i];
        endfunction
        if(find(notDeq, idxVec) matches tagged Valid .idx) begin
            // idx is the first port that does not deq
            // update firstWay (XXX we require SupSize to be power of 2)
            firstDeqWay <= firstDeqWay + idx;
            // update deq day
            deqTime <= deqTime + zeroExtend(idx);
            // sanity check: deq ports[idx..max] are not enabled
            for(Integer i = 0; i < valueof(SupSize); i = i+1) begin
                doAssert((fromInteger(i) < idx) == deqEn[i], "Deq must be consective");
            end
        end
        else begin
            // all ports are dequeued, so firstWay keeps the same
            // update deq day
            deqTime <= deqTime + fromInteger(valueof(SupSize));
        end

        // -- process wrongSpec --
        if(wrongSpecEn.wget matches tagged Valid .x) begin
            SpecTag specTag = x.specTag;
            InstTag killInstTag = x.killInstTag;

            // only update valid, no need to change spec bits
            for(Integer w = 0; w < valueof(SupSize); w = w+1) begin
                for(Integer i = 0; i < valueof(SingleScalarSize); i = i+1) begin
                    if(row[w][i].dependsOn_wrongSpec(specTag)) begin
                        valid[w][i][valid_wrongSpec_port] <= False;
                    end
                end
            end

            // move enqP to be right after OR just the inst that initiates the kill
            // To do this, we need to figure out the number of inst killed in each FIFO way
            // Notice that each FIFO way is enq in round-robin order
            // Since we know the number of killed inst in the FIFO that contains the kill-initiating inst,
            // we can deduce the number of killed inst in other FIFOs
            // For simplicity, we map the way select of FIFOs to a virtual way select
            // such that firstEnqWay will be 0 in the virtual way select
            // i.e. virtual way = way - firstEnqWay XXX requires SupSize to be power of 2
            function SupWaySel toVirtualWay(SupWaySel realWay) = realWay - firstEnqWay;
            // In terms of virtual ways, if we align the enqP of all FIFOs,
            // we know the program order of inst when we move the ptr backward from enqP
            // Thus we can deduce how many inst are killed in each FIFO
            // First consider the FIFO that contains the kill-initiating entry
            // We get virtual way and the distance from kill-initiating entry to enqP
            // With this distance, we can deduce how much we should decrement the enqP for all FIFOs
            SupWaySel virtualKillWay = toVirtualWay(killInstTag.way);
            SingleScalarPtr killEnqP = enqP[killInstTag.way];
            SingleScalarLen killDistToEnqP = killInstTag.ptr < killEnqP ? // if >=, then FIFO must wrap around
                zeroExtend(killEnqP - killInstTag.ptr) :
                zeroExtend(killEnqP) + fromInteger(valueof(SingleScalarSize)) - zeroExtend(killInstTag.ptr);
            doAssert(killDistToEnqP > 0, "distance to enqP must be > 0");
            // helper function to decr enqP
            function SingleScalarPtr decrPtr(SingleScalarPtr ptr, SingleScalarLen len);
                if(zeroExtend(ptr) < len) begin
                    SingleScalarLen extendedPtr = zeroExtend(ptr) + fromInteger(valueof(SingleScalarSize));
                    return truncate(extendedPtr - len);
                end
                else begin
                    return ptr - truncate(len);
                end
            endfunction
            // split into two cases: whether the kill-initialting entry is killed by itself
            // (note: a dequeued entry which calles incorrecSpec is NOT killing itself)
            SupWaySel firstEnqWayNext;
            Vector#(SupSize, SingleScalarPtr) enqPNext;
            Vector#(SupSize, SingleScalarLen) distToEnqP; // amount to decr enqP to get enqPNext, record for debugging
            Bool killSelf = valid[killInstTag.way][killInstTag.ptr][valid_wrongSpec_port] &&
                            row[killInstTag.way][killInstTag.ptr].dependsOn_wrongSpec(specTag);
            if(killSelf) begin
                // the kill-initiating inst also kills itself (e.g. a Ld)
                // so the kill-initiating entry becomes the next enq position
                // first enq way will become the way that contains the kill-initialting entry
                firstEnqWayNext = killInstTag.way;
                // get the enq pointers for all FIFOs
                for(Integer i = 0; i < valueof(SupSize); i = i+1) begin
                    // consider virtual way of FIFO i, and get the distance that enqP should decr
                    // virtual way >= kill virtual way: enqP decr by killDistToEnqP
                    // virtual way <  kill virtual way: enqP decr by killDistToEnqP - 1
                    SupWaySel virtualWay = toVirtualWay(fromInteger(i));
                    distToEnqP[i] = virtualWay >= virtualKillWay ? killDistToEnqP : killDistToEnqP - 1;
                    enqPNext[i] = decrPtr(enqP[i], distToEnqP[i]);
                end
            end
            else begin
                // the kill-initiating inst does not kill itself
                // so the ROB entry entry right after the kill-initiating entry should be the enq position
                // it must be in the way right after the way that contians the kill-initiating entry
                firstEnqWayNext = killInstTag.way + 1;
                // get the enq pointers for all FIFOs
                for(Integer i = 0; i < valueof(SupSize); i = i+1) begin
                    // consider virtual way of FIFO i, and get the distance that enqP should decr
                    // virtual way >  kill virtual way: enqP decr by killDistToEnqP
                    // virtual way <= kill virtual way: enqP decr by killDistToEnqP - 1
                    SupWaySel virtualWay = toVirtualWay(fromInteger(i));
                    distToEnqP[i] = virtualWay > virtualKillWay ? killDistToEnqP : killDistToEnqP - 1;
                    enqPNext[i] = decrPtr(enqP[i], distToEnqP[i]);
                end
            end

            // move enq day to be right after OR just the inst that initiates the kill
            InstTime enqTimeNext;
            if(killSelf) begin
                // kill-initiating entry kills itself, so enqAge becomes the age of killing inst
                enqTimeNext = killInstTag.t;
            end
            else begin
                // kill-initiating entry does not kill itself, so enqAge becomes
                enqTimeNext = killInstTag.t + 1;
            end

            // record state update
            wrongSpecEnqUpdate.wset(ROBWrongSpecEnqUpdate {
                firstEnqWay: firstEnqWayNext,
                enqP: enqPNext,
                enqTime: enqTimeNext
            });

`ifdef BSIM
            // sanity check in simulation
            Vector#(SupSize, SingleScalarPtr) deqPVec = readVEhr(1, deqP_ehr);
            Vector#(SupSize, Vector#(SingleScalarSize, Bool)) depVec;
            Vector#(SupSize, Vector#(SingleScalarSize, Bool)) validVec;
            for(Integer w = 0; w < valueof(SupSize); w = w+1) begin
                validVec[w] = readVEhr(valid_wrongSpec_port, valid[w]);
                function Bool getDepOn(Integer i) = row[w][i].dependsOn_wrongSpec(specTag);
                depVec[w] = map(getDepOn, genVector);
            end
            $display("[ROB incorrectSpec] ",
                fshow(specTag), " ; ",
                fshow(killInstTag), " ; ",
                fshow(firstEnqWay), " ; ",
                fshow(firstDeqWay), " ; ",
                fshow(readVReg(enqP)), " ; ",
                fshow(deqPVec), " ; ",
                fshow(validVec), " ; ",
                fshow(depVec), " ; ",
                fshow(firstEnqWayNext), " ; ",
                fshow(enqPNext), " ; ",
                fshow(distToEnqP)
            );
            // valid entries within [enqPNext, enqP) are killed
            for(Integer w = 0; w < valueof(SupSize); w = w+1) begin
                function Bool in_kill_range(SingleScalarPtr i);
                    if(distToEnqP[w] == 0) begin
                        return False;
                    end
                    else begin
                        if(enqPNext[w] < enqP[w]) begin
                            return enqPNext[w] <= i && i < enqP[w];
                        end
                        else begin
                            return enqPNext[w] <= i || i < enqP[w];
                        end
                    end
                endfunction
                for(Integer i = 0; i < valueof(SingleScalarSize); i = i+1) begin
                    doAssert(
                        in_kill_range(fromInteger(i)) ==
                        (row[w][i].dependsOn_wrongSpec(specTag) && valid[w][i][valid_wrongSpec_port]),
                        "valid entries inside [enqPNext, enqP) must be killed, outsiders must not"
                    );
                end
            end
            // kill-initiating entry may be just dequeued
            if(!valid[killInstTag.way][killInstTag.ptr][valid_wrongSpec_port]) begin
                doAssert(getNextPtr(killInstTag.ptr) == deqPVec[killInstTag.way],
                    "if the kill-initiating entry is invalid, it must be just dequeued"
                );
            end
`endif
        end
    endrule

    // apply enq/wrongSpec effects & update firstEnqWay
    // This rule cannot be merged with canon_deq_wrongSpec,
    // because many other methods access ROB row contents are sandwiched between
    // canon_deq_wrongSpec and this rule
    (* fire_when_enabled, no_implicit_conditions *)
    rule canon_enq;
        if(wrongSpecEnqUpdate.wget matches tagged Valid .upd) begin
            // wrong spec is conflicting with enq, so enqEn must be all false
            for(Integer i = 0; i < valueof(SupSize); i = i+1) begin
                doAssert(!isValid(enqEn[i].wget), "when wrongSpec, enq cannot fire");
            end
            // do update (computation is done in incorrectSpeculation method)
            firstEnqWay <= upd.firstEnqWay;
            for(Integer i = 0; i < valueof(SupSize); i = i+1) begin
                enqP[i] <= upd.enqP[i];
            end
            enqTime <= upd.enqTime;
        end
        else begin
            // no wrong spec, apply effects of enq
            for(Integer i = 0; i < valueof(SupSize); i = i+1) begin
                // for FIFO way i (looping for FIFO way should save area than looping for enq port)
                SupWaySel enqPort = getEnqPort(fromInteger(i));
                doAssert(getEnqFifoWay(enqPort) == fromInteger(i), "enq port matches FIFO way");
                if(enqEn[enqPort].wget matches tagged Valid .x) begin
                    doAssert(!valid[i][enqP[i]][valid_enq_port], "enq entry must be invalid");
                    // update row, set valid, move enqP
                    enqP[i] <= getNextPtr(enqP[i]);
                    row[i][enqP[i]].write_enq(x);
                    valid[i][enqP[i]][valid_enq_port] <= True;
                end
            end
            // update firstEnqWay: find the first enq port that is not enabled
            Vector#(SupSize, SupWaySel) idxVec = genWith(fromInteger);
            function Bool notEnq(SupWaySel i);
                return !isValid(enqEn[i].wget);
            endfunction
            if(find(notEnq, idxVec) matches tagged Valid .idx) begin
                // idx is the first port that does not enq
                // update firstWay (XXX we require SupSize to be power of 2)
                firstEnqWay <= firstEnqWay + idx;
                // update enq day
                enqTime <= enqTime + zeroExtend(idx);
                // sanity check: enq ports[idx..max] are not enabled
                for(Integer i = 0; i < valueof(SupSize); i = i+1) begin
                    doAssert((fromInteger(i) < idx) == isValid(enqEn[i].wget), "Enq must be consecutive");
                end
            end
            else begin
                // all ports enq, so firstWay keeps the same
                // update enq day
                enqTime <= enqTime + fromInteger(valueof(SupSize));
            end
        end
    endrule

`ifdef BSIM
    // sanity check in simulation
    // all valid entry are within [deqP, enqP), outsiders are invalid entries
    (* fire_when_enabled, no_implicit_conditions *)
    rule sanityCheck;
        for(Integer w = 0; w < valueof(SupSize); w = w+1) begin
            Bool empty = all( \== (False), readVEhr(0, valid[w]) );
            function Bool in_range(SingleScalarPtr i);
                // i is within [deqP, enqP)
                if(empty) begin
                    return False;
                end
                else begin
                    if(deqP[w] < enqP[w]) begin
                        return deqP[w] <= i && i < enqP[w];
                    end
                    else begin
                        return deqP[w] <= i || i < enqP[w];
                    end
                end
            endfunction
            for(Integer i = 0; i < valueof(SingleScalarSize); i = i+1) begin
                doAssert(in_range(fromInteger(i)) == valid[w][i][0],
                    "entries inside [deqP, enqP) should be valid, otherwise invalid"
                );
            end
        end
    endrule
`endif

    // get enq ifc
    // we compute can_enq signal for each FIFO[i], and all FIFO empty signal lazily
    staticAssert(lazyEnq, "Only support lazy enq");
    Vector#(SupSize, Wire#(Bool)) can_enq_fifo <- replicateM(mkBypassWire); // FIFO[i] can enq (enq slot invalid)
    Wire#(Bool) empty_for_enq <- mkBypassWire; // all FIFOs empty

    (* fire_when_enabled, no_implicit_conditions *)
    rule setEnqWires;
        Vector#(SupSize, Integer) idxVec = genVector;
        // get all empty
        function Bool isEmptyFunc(Integer i);
            return !valid[i][enqP[i]][0] && enqP[i] == deqP[i];
        endfunction
        empty_for_enq <= all(isEmptyFunc, idxVec);
        // get can enq
        function Action setCanEnq(Integer i);
        action
            can_enq_fifo[i] <= !valid[i][enqP[i]][0];
        endaction
        endfunction
        joinActions(map(setCanEnq, idxVec));
    endrule

    Vector#(SupSize, ROB_EnqPort) enqIfc;
    for(Integer i = 0; i < valueof(SupSize); i = i+1) begin
        SupWaySel way = getEnqFifoWay(fromInteger(i)); // FIFO[way] is used by enq port i
        Bool can_enq = can_enq_fifo[way];
        enqIfc[i] = (interface ROB_EnqPort;
            method Bool canEnq = can_enq;
            method Action enq(ToReorderBuffer x) if(can_enq);
                doAssert(getEnqPort(way) == fromInteger(i), "enq FIFO way matches enq port");
                // record enq action, real action is applied later
                enqEn[i].wset(x);
                // make it conflict with wrong speculation
                wrongSpec_enq_conflict[i].wset(?);
                // ordering: sequence after many other methods
                deq_SB_enq[i] <= False;
                setExeAlu_SB_enq[i] <= False;
                setExeMem_SB_enq[i] <= False;
                setExeFpuMulDiv_SB_enq[i] <= False;
                setExeLSQ_SB_enq[i] <= False;
                setLdSB_SB_enq[i] <= False;
            endmethod
            method InstTag getEnqInstTag;
                return InstTag {
                    way: way,
                    ptr: enqP[way],
                    t: enqTime + fromInteger(i)
                };
            endmethod
        endinterface);
    end

    // get deq ifc
    // get canDeq & first for each FIFO[i]
    function Bool getFifoCanDeq(Integer i) = valid[i][deqP[i]][valid_deq_port];
    function ToReorderBuffer getFifoFirst(Integer i) = row[i][deqP[i]].read_deq;
    Vector#(SupSize, Bool) can_deq_fifo = map(getFifoCanDeq, genVector);
    Vector#(SupSize, ToReorderBuffer) fifo_first = map(getFifoFirst, genVector);

    Vector#(SupSize, ROB_DeqPort) deqIfc;
    for(Integer i = 0; i < valueof(SupSize); i = i+1) begin
        SupWaySel way = getDeqFifoWay(fromInteger(i)); // FIFO[way] is used by deq port i
        Bool can_deq = can_deq_fifo[way] && all(id, readVReg(deq_SB_enq)); // ordering: < enq
        deqIfc[i] = (interface ROB_DeqPort;
            method Bool canDeq = can_deq;
            method Action deq if(can_deq);
                doAssert(getDeqPort(way) == fromInteger(i), "deq FIFO way matches deq port");
                deqEn[i].send; // record deq action, real action is applied later
            endmethod
            method ToReorderBuffer deq_data if(can_deq);
                return fifo_first[way];
            endmethod
            method InstTag getDeqInstTag;
                return InstTag {
                    way: way,
                    ptr: deqP[way],
                    t: deqTime + fromInteger(i)
                };
            endmethod
        endinterface);
    end

    // set alu exe ifc
    Vector#(aluExeNum, ROB_setExecuted_doFinishAlu) aluSetExeIfc;
    for(Integer i = 0; i < valueof(aluExeNum); i = i+1) begin
        aluSetExeIfc[i] = (interface ROB_setExecuted_doFinishAlu;
            method Action set(
                InstTag x, Data res, Maybe#(Data) csrData, ControlFlow cf, RobInstState new_state
            ) if(
                all(id, readVReg(setExeAlu_SB_enq)) // ordering: < enq
            );
                row[x.way][x.ptr].setExecuted_doFinishAlu[i].set(res, csrData, cf, new_state);
            endmethod
        endinterface);
    end

    // get pc/ppc ifc used by alu exe
    Vector#(aluExeNum, ROB_getOrigPC) getOrigPCIfc;
    Vector#(aluExeNum, ROB_getOrigPredPC) getOrigPredPCIfc;
    for(Integer i = 0; i < valueof(aluExeNum); i = i+1) begin
        getOrigPCIfc[i] = (interface ROB_getOrigPC;
            method Addr get(InstTag x) = row[x.way][x.ptr].getOrigPC;
        endinterface);
        getOrigPredPCIfc[i] = (interface ROB_getOrigPredPC;
            method Addr get(InstTag x) = row[x.way][x.ptr].getOrigPredPC;
        endinterface);
    end

    interface enqPort = enqIfc;

    method Bool isEmpty;
        return empty_for_enq;
    endmethod

    interface deqPort = deqIfc;

    method Bool isEmpty_ehrPort0;
        function Bool isEmptyFunc(Integer i);
            return !valid[i][enqP[i]][0] && enqP[i] == deqP[i];
        endfunction
        Vector#(SupSize, Integer) idxVec = genVector;
        return all(isEmptyFunc, idxVec);
    endmethod

    method Bool isFull_ehrPort0;
        function Bool isFullFunc(Integer i);
            return valid[i][enqP[i]][0] && enqP[i] == deqP[i];
        endfunction
        Vector#(SupSize, Integer) idxVec = genVector;
        return all(isFullFunc, idxVec);
    endmethod

    method Action setExecuted_deqLSQ(InstTag x, Data res, RobInstState new_state) if(
        all(id, readVReg(setExeLSQ_SB_enq)) // ordering: < enq
    );
        row[x.way][x.ptr].setExecuted_deqLSQ(res, new_state);
    endmethod

    interface setExecuted_doFinishAlu = aluSetExeIfc;

    method Action setExecuted_doFinishFpuMulDiv(
        InstTag x, Data res, Bit#(5) fflags, RobInstState new_state
    ) if(
        all(id, readVReg(setExeFpuMulDiv_SB_enq)) // ordering: < enq
    );
        row[x.way][x.ptr].setExecuted_doFinishFpuMulDiv(res, fflags, new_state);
    endmethod

    method Action setExecuted_doFinishMem(
        InstTag x, Data data, Addr vaddr, Maybe#(Exception) cause, RobInstState new_state
    ) if(
        all(id, readVReg(setExeMem_SB_enq)) // ordering: < enq
    );
        row[x.way][x.ptr].setExecuted_doFinishMem(data, vaddr, cause, new_state);
    endmethod

    method Action setLdSpecBit(InstTag x, SpecTag ldSpecTag) if(
        all(id, readVReg(setLdSB_SB_enq)) // ordering: < enq
    );
        row[x.way][x.ptr].setLdSpecBit(ldSpecTag);
    endmethod

    interface getOrigPC = getOrigPCIfc;
    interface getOrigPredPC = getOrigPredPCIfc;
    //method Maybe#(RenamingTag) getRenameTag(InstTag x) = row[x.way][x.ptr].getRenameTag;

    method InstTime getEnqTime = enqTime;

    interface ROB_SpeculationUpdate specUpdate;
        method Action correctSpeculation(SpecBits mask);
            for(Integer w = 0; w < valueof(SupSize); w = w+1) begin
                for(Integer i = 0; i < valueof(SingleScalarSize); i = i+1) begin
                    row[w][i].correctSpeculation(mask);
                end
            end
        endmethod

        method Action incorrectSpeculation(SpecTag specTag, InstTag killInstTag);
            // record wrongSpec action
            wrongSpecEn.wset(ROBWrongSpecInput {
                specTag: specTag,
                killInstTag: killInstTag
            });
            // make it conflict with enq
            for(Integer i = 0; i < valueof(SupSize); i = i+1) begin
                wrongSpec_enq_conflict[i].wset(?);
            end
        endmethod
    endinterface
endmodule
`endif
