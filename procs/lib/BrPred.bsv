import Types::*;
import ProcTypes::*;
import Vector::*;

(* noinline *)
function Maybe#(Addr) decodeBrPred( Addr pc, DecodedInst dInst, Bool histTaken );
  Addr pcPlus4 = pc + 4;
  Data imm_val = fromMaybe(?, getDInstImm(dInst));
  Maybe#(Addr) nextPc = tagged Invalid;
  if( dInst.iType == J ) begin
    Addr jTarget = pc + imm_val;
    nextPc = tagged Valid jTarget;
  end else if( dInst.iType == Br ) begin
    if( histTaken ) begin
      nextPc = tagged Valid (pc + imm_val);
    end else begin
      nextPc = tagged Valid pcPlus4;
    end
  end else if( dInst.iType == Jr ) begin
    // target is unknown until RegFetch
    nextPc = tagged Invalid;
  end else begin
    nextPc = tagged Valid pcPlus4;
  end
  return nextPc;
endfunction

// general types for direction predictor

typedef struct {
    Bool taken;
    trainInfoT train; // info that a branch must keep for future training
} DirPredResult#(type trainInfoT) deriving(Bits, Eq, FShow);

interface DirPred#(type trainInfoT);
    method ActionValue#(DirPredResult#(trainInfoT)) pred(Addr pc);
endinterface

interface DirPredictor#(type trainInfoT);
    interface Vector#(SupSize, DirPred#(trainInfoT)) pred;
    method Action update(Addr pc, Bool taken, trainInfoT train, Bool mispred);
endinterface

