import Types::*;
import ProcTypes::*;
import Core::*;
import CrossBar::*;
import Vector::*;
import GetPut::*;

module mkIPIConnect#(Vector#(CoreNum, CoreIPI) ipi)(Empty);
    Vector#(CoreNum, Get#(CoreId)) src = ?;
    Vector#(CoreNum, Put#(void)) dst = ?;
    for(Integer i = 0; i < valueof(CoreNum); i = i+1) begin
        src[i] = toGet(ipi[i].sendIPI);
        dst[i] = (interface Put;
            method Action put(void x);
                ipi[i].recvIPI;
            endmethod
        endinterface);
    end

    function XBarDstInfo#(CoreId, void) getDst(CoreId srcIdx, CoreId srcData);
        return XBarDstInfo {idx: srcData, data: ?};
    endfunction
    mkXBar(getDst, src, dst);
endmodule
