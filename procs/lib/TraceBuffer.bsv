import BRAMFIFO::*;
import Assert::*;
import FIFO::*;
import FIFOF::*;
import Cntrs::*;
import Vector::*;
import GetPut::*;
import ConfigReg::*;
import Types::*;

interface TraceBuffer#(numeric type wayNum, type traceT);
    interface Vector#(wayNum, Put#(traceT)) enq;
    interface Get#(Tuple2#(Data, traceT)) deq; // clk + trace
    method ActionValue#(Data) drop; // clk when a trace is dropped
    method Action trigger; // deadlock detected, start sending buffered data to host
endinterface

typedef 1024 TraceSize;
typedef Bit#(TLog#(TAdd#(1, TraceSize))) TraceCnt;

module mkTraceBuffer#(
    ReadOnly#(Data) clk, Integer postCnt // number of traces accepted after trigger
)(TraceBuffer#(wayNum, traceT)) provisos(
    Bits#(traceT, a__), Bits#(Tuple2#(Data, traceT), b__)
);
    // require at least 1 post cnt
    staticAssert(postCnt >= 1 && postCnt < valueof(TraceSize), "invalid post cnt");

    FIFOF#(Tuple2#(Data, Vector#(wayNum, Maybe#(traceT)))) traceQ <- mkSizedBRAMFIFOF(valueof(TraceSize));
    // we maintain the number of entries in trace Q <= TraceSize - 1, so enq to
    // traceQ is always ready. This is only valid before trigger is called
    Reg#(TraceCnt) traceCnt <- mkReg(0);
    PulseWire incTraceCnt <- mkPulseWire;
    PulseWire decTraceCnt <- mkPulseWire;

    // whether the bug has been triggered
    Reg#(Bool) triggered <- mkConfigReg(False);

    // enq wires
    Vector#(wayNum, RWire#(traceT)) enqEn <- replicateM(mkRWire);

    // drop trace
    Reg#(Maybe#(Data)) dropTime <- mkConfigReg(Invalid);
    Reg#(Bool) dropSent <- mkReg(False);

    // deq FIFO & ptr
    Reg#(Bit#(TLog#(wayNum))) deqPtr <- mkReg(0);
    FIFO#(Tuple2#(Data, traceT)) deqQ <- mkFIFO;

    (* fire_when_enabled *)
    rule canonEnq;
        function Maybe#(traceT) getWire(RWire#(traceT) w) = w.wget;
        Vector#(wayNum, Maybe#(traceT)) enqVals = map(getWire, enqEn);
        if(any(isValid, enqVals)) begin
            if(!triggered || traceCnt < fromInteger(valueof(TraceSize))) begin
                if(traceQ.notFull) begin
                    traceQ.enq(tuple2(clk, enqVals));
                    incTraceCnt.send;
                end
                else begin
                    // drop trace
                    if(!isValid(dropTime)) begin
                        dropTime <= Valid (clk);
                    end
                end
            end
            // else: ignore additional traces after trigger
        end
    endrule

    (* fire_when_enabled *)
    rule deleteOldTrace(!triggered && traceCnt >= fromInteger(valueof(TraceSize) - postCnt));
        traceQ.deq;
        decTraceCnt.send;
    endrule

    (* fire_when_enabled, no_implicit_conditions *)
    rule canonTraceCnt;
        if(decTraceCnt && !incTraceCnt) begin
            traceCnt <= traceCnt - 1;
        end
        else if(incTraceCnt && !decTraceCnt) begin
            traceCnt <= traceCnt + 1;
        end
    endrule

    rule sendDeq(triggered);
        let {cyc, traceVec} = traceQ.first;
        if(traceVec[deqPtr] matches tagged Valid .trace) begin
            deqQ.enq(tuple2(cyc, trace));
        end
        if(deqPtr == fromInteger(valueof(wayNum) - 1)) begin
            deqPtr <= 0;
            traceQ.deq;
        end
        else begin
            deqPtr <= deqPtr + 1;
        end
    endrule

    function Put#(traceT) setWire(RWire#(traceT) w);
        return (interface Put;
            method put = w.wset;
        endinterface);
    endfunction

    interface enq = map(setWire, enqEn);

    interface deq = toGet(deqQ);

    method ActionValue#(Data) drop if(dropTime matches tagged Valid .t &&& !dropSent);
        dropSent <= True;
        return t;
    endmethod

    method Action trigger;
        triggered <= True;
    endmethod
endmodule
