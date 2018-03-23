import DefaultValue::*;
import GetPut::*;
import ClientServer::*;
import FloatingPoint::*;
import FIFO::*;

// dummy FPU

typedef FloatingPoint::RoundMode FpuRoundMode;
typedef FloatingPoint::Exception FpuException;

(* synthesize *)
module mkXilinxFpFma(Server#(
    Tuple4#(Maybe#(Double), Double, Double, FpuRoundMode),
    Tuple2#(Double, FpuException)
));
    FIFO#(Tuple4#(Maybe#(Double), Double, Double, FpuRoundMode)) inQ <- mkFIFO;
    FIFO#(Tuple2#(Double, FpuException)) outQ <- mkFIFO;

    rule doReq;
        let {d1_maybe, d2, d3, rm} <- toGet(inQ).get;
        let d1 = fromMaybe(0, d1_maybe);
        let val = pack(d1) & pack(d2) & pack(d3) & zeroExtend(pack(rm));
        let exc = pack(d1) | pack(d2) | pack(d3) | zeroExtend(pack(rm));
        outQ.enq(tuple2(unpack(val), unpack(truncate(exc))));
    endrule

    interface request = toPut(inQ);
    interface response = toGet(outQ);
endmodule

(* synthesize *)
module mkXilinxFpDiv(Server#(
    Tuple3#(Double, Double, FpuRoundMode),
    Tuple2#(Double, FpuException)
));
    FIFO#(Tuple3#(Double, Double, FpuRoundMode)) inQ <- mkFIFO;
    FIFO#(Tuple2#(Double, FpuException)) outQ <- mkFIFO;

    rule doReq;
        let {d1, d2, rm} <- toGet(inQ).get;
        let val = pack(d1) & pack(d2) & zeroExtend(pack(rm));
        let exc = pack(d1) | pack(d2) | zeroExtend(pack(rm));
        outQ.enq(tuple2(unpack(val), unpack(truncate(exc))));
    endrule

    interface request = toPut(inQ);
    interface response = toGet(outQ);
endmodule

(* synthesize *)
module mkXilinxFpSqrt(Server#(
    Tuple2#(Double, FpuRoundMode),
    Tuple2#(Double, FpuException)
));
    FIFO#(Tuple2#(Double, FpuRoundMode)) inQ <- mkFIFO;
    FIFO#(Tuple2#(Double, FpuException)) outQ <- mkFIFO;

    rule doReq;
        let {d1, rm} <- toGet(inQ).get;
        let val = pack(d1) & zeroExtend(pack(rm));
        let exc = pack(d1) | zeroExtend(pack(rm));
        outQ.enq(tuple2(unpack(val), unpack(truncate(exc))));
    endrule

    interface request = toPut(inQ);
    interface response = toGet(outQ);
endmodule
