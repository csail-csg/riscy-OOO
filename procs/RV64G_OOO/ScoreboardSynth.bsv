import Scoreboard::*;
import SynthParam::*;

// conservative sb
typedef RenamingScoreboard#(WrConsPortNum, SbLazyLookupPortNum) ScoreboardCons;
(* synthesize *)
module mkScoreboardCons(ScoreboardCons);
    let m <- mkRenamingScoreboard;
    return m;
endmodule

// aggressive sb
typedef RenamingScoreboard#(WrAggrPortNum, 0) ScoreboardAggr;
(* synthesize *)
module mkScoreboardAggr(ScoreboardAggr);
    let m <- mkRenamingScoreboard;
    return m;
endmodule

