import Types::*;
import ProcTypes::*;
import RegFile::*;
import Vector::*;
import BrPred::*;

export BhtTrainInfo;
export mkBht;

typedef Bit#(0) BhtTrainInfo; // no training info needs to be remembered

// Local BHT Typedefs
typedef 128 BhtEntries;
typedef Bit#(TLog#(BhtEntries)) BhtIndex;

(* synthesize *)
module mkBht(DirPredictor#(BhtTrainInfo));
    // Read and Write ordering doesn't matter since this is a predictor
    // mkRegFileWCF is the RegFile version of mkConfigReg
    RegFile#(BhtIndex, Bit#(2)) hist <- mkRegFileWCF(0,fromInteger(valueOf(BhtEntries)-1));

    function BhtIndex getIndex(Addr pc);
        return truncate(pc >> 2);
    endfunction

    Vector#(SupSize, DirPred#(BhtTrainInfo)) predIfc;
    for(Integer i = 0; i < valueof(SupSize); i = i+1) begin
        predIfc[i] = (interface DirPred;
            method ActionValue#(DirPredResult#(BhtTrainInfo)) pred(Addr pc);
                let index = getIndex(pc);
                Bit#(2) cnt = hist.sub(index);
                Bool taken = cnt[1] == 1;
                return DirPredResult {
                    taken: taken,
                    train: 0
                };
            endmethod
        endinterface);
    end

    interface pred = predIfc;

    method Action update(Addr pc, Bool taken, BhtTrainInfo train, Bool mispred);
        let index = getIndex(pc);
        let current_hist = hist.sub(index);
        Bit#(2) next_hist;
        if(taken) begin
            next_hist = (current_hist == 2'b11) ? 2'b11 : current_hist + 1;
        end else begin
            next_hist = (current_hist == 2'b00) ? 2'b00 : current_hist - 1;
        end
        hist.upd(index, next_hist);
    endmethod
endmodule

