import Types::*;
import ProcTypes::*;
import Vector::*;

interface SendBypass;
    method Action send(PhyRIndx dst, Data data);
endinterface

interface RecvBypass;
    method Action recv(PhyRIndx dst, Data data);
endinterface

function ActionValue#(Data) readRFBypass(
    PhyRIndx src,
    Bool rfReady, Data rfData,
    Vector#(n, RWire#(Tuple2#(PhyRIndx, Data))) bypassWire
);
actionvalue
    if(rfReady) begin
        return rfData;
    end
    else begin
        // search through all bypass
        function Maybe#(Data) getBypassData(RWire#(Tuple2#(PhyRIndx, Data)) w);
            if(w.wget matches tagged Valid {.dst, .d} &&& dst == src) begin
                return Valid (d);
            end
            else begin
                return Invalid;
            end
        endfunction
        Vector#(n, Maybe#(Data)) bypassData = map(getBypassData, bypassWire);
        if(find(isValid, bypassData) matches tagged Valid .b) begin
            doAssert(isValid(b), "bypass found must be valid");
            return validValue(b);
        end
        else begin
            when(False, noAction);
            return ?;
        end
    end
endactionvalue
endfunction

function RecvBypass getRecvBypassIfc(RWire#(Tuple2#(PhyRIndx, Data)) w);
    return (interface RecvBypass;
        method Action recv(PhyRIndx dst, Data data);
            w.wset(tuple2(dst, data));
        endmethod
    endinterface);
endfunction

