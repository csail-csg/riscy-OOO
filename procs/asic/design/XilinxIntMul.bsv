import Vector::*;
import FIFOF::*;
import FIFO::*;
import Assert::*;
import GetPut::*;

typedef enum {
    Signed,
    Unsigned,
    SignedUnsigned
} XilinxIntMulSign deriving(Bits, Eq, FShow);

// wrap up all mul IPs to have back pressure
interface XilinxIntMul#(type tagT);
    method Action req(Bit#(64) a, Bit#(64) b, XilinxIntMulSign sign, tagT tag);
    method Action deqResp;
    method Bool respValid;
    method Bit#(128) product;
    method tagT respTag;
endinterface

module mkXilinxIntMul(XilinxIntMul#(tagT)) provisos(
    Bits#(tagT, tagSz)
);

    FIFO#(Tuple4#(Bit#(64), Bit#(64), XilinxIntMulSign, tagT)) inQ <- mkFIFO;
    FIFOF#(Tuple2#(Bit#(128), tagT)) outQ <- mkFIFOF;

    rule doReq;
        let {a, b, sign, tag} <- toGet(inQ).get;
        let prod = {a + zeroExtend(pack(sign)), b + zeroExtend(pack(sign))};
        outQ.enq(tuple2(prod, tag));
    endrule

    method Action req(Bit#(64) a, Bit#(64) b, XilinxIntMulSign sign, tagT tag);
        inQ.enq(tuple4(a, b, sign, tag));
    endmethod

    method Action deqResp;
        outQ.deq;
    endmethod

    method respValid = outQ.notEmpty;

    method Bit#(128) product;
        return tpl_1(outQ.first);
    endmethod

    method tagT respTag;
        return tpl_2(outQ.first);
    endmethod
endmodule

