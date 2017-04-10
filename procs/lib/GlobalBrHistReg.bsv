import Types::*;
import ProcTypes::*;

export GlobalBrHistReg(..);
export mkGlobalBrHistReg;

interface GlobalBrHistReg#(numeric type histLen);
    method Bit#(histLen) history;
    // add new history: taken[0--num-1] are newly added history
    // taken[num-1] is the latest branch
    method Action addHistory(Bit#(SupSize) taken, Bit#(TLog#(TAdd#(SupSize, 1))) num);
    method Action redirect(Bit#(histLen) newHist);
endinterface

typedef struct {
    Bit#(SupSize) taken;
    Bit#(TLog#(TAdd#(SupSize, 1))) num;
} AddHistory deriving(Bits, Eq, FShow);

module mkGlobalBrHistReg(GlobalBrHistReg#(histLen));
    // history reg: MSB is the most recent branch
    Reg#(Bit#(histLen)) hist <- mkReg(0);

    // wires to change history
    RWire#(AddHistory) addHist <- mkRWire; // used by addHistory
    RWire#(Bit#(histLen)) redirectHist <- mkRWire; // used by redirect

    (* fire_when_enabled, no_implicit_conditions *)
    rule canon_redirect(isValid(redirectHist.wget));
        hist <= validValue(redirectHist.wget);
    endrule

    (* fire_when_enabled, no_implicit_conditions *)
    rule canon_addHistory(!isValid(redirectHist.wget) && isValid(addHist.wget));
        let x = validValue(addHist.wget);
        // shift into hist from MSB
        hist <= truncate({x.taken, hist} >> x.num);
    endrule
    
    method history = hist;

    method Action addHistory(Bit#(SupSize) taken, Bit#(TLog#(TAdd#(SupSize, 1))) num);
        addHist.wset(AddHistory {
            taken: taken,
            num: num
        });
    endmethod

    method Action redirect(Bit#(histLen) newHist);
        redirectHist.wset(newHist);
    endmethod
endmodule
