`include "ProcConfig.bsv"
import ClientServer::*;
import Fifo::*;
import GetPut::*;
import MemoryTypes::*;
import ProcTypes::*;
import Types::*;
import Vector::*;

module mkSplitWideMem(  Server#(WideMemReq, WideMemResp) mem_to_proc,
                        Vector#(n, WideMem) ifc );

    Vector#(n, Fifo#(2, WideMemReq)) reqFifos <- replicateM(mkCFFifo);
    Fifo#(TAdd#(n,1), Bit#(TLog#(n))) reqSource <- mkCFFifo;
    Vector#(n, Fifo#(2, WideMemResp)) respFifos <- replicateM(mkCFFifo);

    rule doReq;
        Maybe#(Bit#(TLog#(n))) req_index = tagged Invalid;
        for( Integer i = 0 ; i < valueOf(n) ; i = i+1 ) begin
            if( !isValid(req_index) && reqFifos[i].notEmpty ) begin
                req_index = tagged Valid (fromInteger(i));
            end
        end

        if( isValid(req_index) ) begin
            let req = reqFifos[ fromMaybe(?,req_index) ].first;
            reqFifos[ fromMaybe(?,req_index) ].deq();

            mem_to_proc.request.put(req);
            if( req.op == Ld ) begin
                // req is a load, so keep track of the source
                reqSource.enq( fromMaybe(?,req_index) );
            end
        end
    endrule

    rule doResp;
        let resp <- mem_to_proc.response.get;

        let source = reqSource.first;
        reqSource.deq;
	respFifos[source].enq( resp );
    endrule

    Vector#(n, WideMem) wideMemIfcs = newVector;
    for( Integer i = 0 ; i < valueOf(n) ; i = i+1 ) begin
        wideMemIfcs[i] =
            (interface WideMem;
                interface Server to_proc;
                    interface Put request;
                        method Action put( WideMemReq x );
                            reqFifos[i].enq(x);
                        endmethod
                    endinterface
                    interface Get response;
                        method ActionValue#(WideMemResp) get;
                            let x = respFifos[i].first;
                            respFifos[i].deq;
                            return x;
                        endmethod
                    endinterface
                endinterface
            endinterface);
    end
    return wideMemIfcs;
endmodule
