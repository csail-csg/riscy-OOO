import FIFOF::*;
import FIFO::*;
import GetPut::*;

export XilinxIntDiv(..);
export mkXilinxIntDiv;

// import Xilinx IP core for unsigned division

// axi tuser: user tag + info needed to handle divide by 0 + sign info
typedef struct {
    Bool divByZero;
    Bit#(64) divByZeroRem; // remainder in case of divide by zero
    Bool signedDiv; // signed division (so dividend/divisor has been abs)
    Bit#(1) quotientSign; // sign bit of quotient (in case of signedDiv)
    Bit#(1) remainderSign; // sign bit of remainder (in case of signedDiv)
    Bit#(8) tag;
} IntDivUser deriving(Bits, Eq, FShow);

interface IntDivUnsignedImport;
    method Action enqDividend(Bit#(64) dividend, IntDivUser user);
    method Action enqDivisor(Bit#(64) divisor);
    method Action deqResp;
    method Bool respValid;
    method Bit#(128) quotient_remainder;
    method IntDivUser respUser;
endinterface

import "BVI" int_div_unsigned =
module mkIntDivUnsignedImport(IntDivUnsignedImport);
    default_clock clk(aclk, (*unused*) unused_gate);
    default_reset no_reset;

    method enqDividend(
        s_axis_dividend_tdata, s_axis_dividend_tuser
    ) enable(s_axis_dividend_tvalid) ready(s_axis_dividend_tready);

    method enqDivisor(
        s_axis_divisor_tdata
    ) enable(s_axis_divisor_tvalid) ready(s_axis_divisor_tready);

    method deqResp() enable(m_axis_dout_tready) ready(m_axis_dout_tvalid);
    method m_axis_dout_tvalid respValid;
    method m_axis_dout_tdata quotient_remainder ready(m_axis_dout_tvalid);
    method m_axis_dout_tuser respUser ready(m_axis_dout_tvalid);

    schedule (enqDividend) C (enqDividend);
    schedule (enqDivisor) C (enqDivisor);
    schedule (deqResp) C (deqResp);
    schedule (enqDividend) CF (enqDivisor, deqResp);
    schedule (enqDivisor) CF (deqResp);
    schedule (
        respValid,
        quotient_remainder,
        respUser
    ) CF (
        respValid,
        quotient_remainder,
        respUser,
        enqDividend,
        enqDivisor,
        deqResp
    );
endmodule

// simulation
module mkIntDivUnsignedSim(IntDivUnsignedImport);
    FIFO#(Tuple2#(Bit#(64), IntDivUser)) dividendQ <- mkFIFO;
    FIFO#(Bit#(64)) divisorQ <- mkFIFO;
    FIFOF#(Tuple2#(Bit#(128), IntDivUser)) respQ <- mkSizedFIFOF(2);

    rule compute;
        dividendQ.deq;
        divisorQ.deq;
        let {dividend, user} = dividendQ.first;
        let divisor = divisorQ.first;

        UInt#(64) a = unpack(dividend);
        UInt#(64) b = unpack(divisor);
        Bit#(64) q = pack(a / b);
        Bit#(64) r = pack(a % b);
        respQ.enq(tuple2({q, r}, user));
    endrule

    method Action enqDividend(Bit#(64) dividend, IntDivUser user);
        dividendQ.enq(tuple2(dividend, user));
    endmethod

    method Action enqDivisor(Bit#(64) divisor);
        divisorQ.enq(divisor);
    endmethod

    method Action deqResp;
        respQ.deq;
    endmethod

    method respValid = respQ.notEmpty;

    method quotient_remainder = tpl_1(respQ.first);

    method respUser = tpl_2(respQ.first);
endmodule

interface XilinxIntDiv#(type tagT);
    method Action req(Bit#(64) dividend, Bit#(64) divisor, Bool signedDiv, tagT tag);
    // response
    method Action deqResp;
    method Bool respValid;
    method Bit#(64) quotient;
    method Bit#(64) remainder;
    method tagT respTag;
endinterface

module mkXilinxIntDiv(XilinxIntDiv#(tagT)) provisos (
    Bits#(tagT, tagSz)
);
    FIFO#(Tuple4#(Bit#(64), Bit#(64), Bool, tagT)) inQ <- mkFIFO;
    FIFOF#(Tuple3#(Bit#(64), Bit#(64), tagT)) outQ <- mkFIFOF;

    rule doReq;
        let {a, b, sign, tag} <- toGet(inQ).get;
        if(sign) begin
            outQ.enq(tuple3(a, b, tag));
        end
        else begin
            outQ.enq(tuple3(b, a, tag));
        end
    endrule

    method Action req(
        Bit#(64) dividend, Bit#(64) divisor, Bool signedDiv, tagT tag
    );
        inQ.enq(tuple4(dividend, divisor, signedDiv, tag));
    endmethod

    method Action deqResp;
        outQ.deq;
    endmethod

    method respValid = outQ.notEmpty;
    
    method Bit#(64) quotient;
        return tpl_1(outQ.first);
    endmethod

    method Bit#(64) remainder;
        return tpl_2(outQ.first);
    endmethod 

    method tagT respTag;
        return tpl_3(outQ.first);
    endmethod
endmodule

