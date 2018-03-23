
// override RWBramCore in coherence repo

import Vector::*;
import ConfigReg::*;
import Fifo::*;

interface RWBramCore#(type addrT, type dataT);
    method Action wrReq(addrT a, dataT d);
    method Action rdReq(addrT a);
    method dataT rdResp;
    method Bool rdRespValid;
    method Action deqRdResp;
endinterface

module mkRWBramCore(RWBramCore#(addrT, dataT)) provisos(
    Bits#(addrT, addrSz), Bits#(dataT, dataSz)
);
    Vector#(2, Reg#(dataT)) data <- replicateM(mkConfigReg(unpack(0)));
    Fifo#(1, dataT) rdRespQ <- mkPipelineFifo;

    // do address selection randomly, but need all info in addrT
    Reg#(Bit#(addrSz)) mask <- mkReg(1);
    rule incrClk;
        Bit#(addrSz) m = mask << 1;
        if(m == 0) begin
            m = 1;
        end
        mask <= m;
    endrule
    function Bit#(1) getSel(addrT a);
        return (pack(a) & mask) == 0 ? 0 : 1;
    endfunction

    method Action wrReq(addrT a, dataT d);
        data[getSel(a)] <= d;
    endmethod

    method Action rdReq(addrT a);
        rdRespQ.enq(data[getSel(a)]);
    endmethod

    method dataT rdResp;
        return rdRespQ.first;
    endmethod

    method rdRespValid = rdRespQ.notEmpty;

    method Action deqRdResp;
        rdRespQ.deq;
    endmethod
endmodule
