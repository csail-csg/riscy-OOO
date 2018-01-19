import Types::*;
import ProcTypes::*;
import Vector::*;

interface ArchRegFile;
    method Data rd(ArchRIndx idx);
    method Action wr(ArchRIndx idx, Data val);
endinterface

module mkArchRegFile(ArchRegFile) provisos(
    NumAlias#(gprNum, TExp#(SizeOf#(GprRIndx))),
    NumAlias#(fpuNum, TExp#(SizeOf#(FpuRIndx)))
);
    Vector#(gprNum, Reg#(Data)) gpr;
    // GPR 0 is always 0
    gpr[0] = (interface Reg;
        method Data _read;
            return 0;
        endmethod
        method Action _write(Data v);
            noAction;
        endmethod
    endinterface);
    for(Integer i = 1; i < valueof(gprNum); i = i + 1) begin
        gpr[i] <- mkReg(0);
    end

    Vector#(fpuNum, Reg#(Data)) fpu <- replicateM(mkReg(0));

    method Data rd(ArchRIndx idx);
        case(idx) matches
            tagged Gpr .i: return gpr[i];
            tagged Fpu .i: return fpu[i];
            default: return 0;
        endcase
    endmethod

    method Action wr(ArchRIndx idx, Data val);
        case(idx) matches
            tagged Gpr .i: gpr[i] <= val;
            tagged Fpu .i: fpu[i] <= val;
            default: noAction;
        endcase
    endmethod
endmodule
