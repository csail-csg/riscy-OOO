import GetPut::*;
import ClientServer::*;
import Connectable::*;
import Types::*;
import ProcTypes::*;
import TlbTypes::*;
import CacheUtils::*;
import ITlb::*;
import DTlb::*;
import L2Tlb::*;

module mkTlbConnect#(ITlbToParent i, DTlbToParent d, L2TlbToChildren l2)(Empty);
    // give priority to DTlb req
    (* descending_urgency = "sendDTlbReq, sendITlbReq" *)
    rule sendDTlbReq;
        DTlbRqToP r <- toGet(d.rqToP).get;
        l2.rqFromC.put(L2TlbRqFromC {
            child: D,
            vpn: r.vpn,
            reqType: r.t,
            write: r.write
        });
    endrule

    rule sendITlbReq;
        ITlbRqToP r <- toGet(i.rqToP).get;
        l2.rqFromC.put(L2TlbRqFromC {
            child: I,
            vpn: r.vpn,
            reqType: LdTranslation,
            write: False
        });
    endrule

    rule sendRsToDTlb(l2.rsToC.first.child == D);
        L2TlbRsToC r <- toGet(l2.rsToC).get;
        d.ldTransRsFromP.enq(DTlbTransRsFromP {entry: r.entry});
    endrule

    rule sendRsToITlb(l2.rsToC.first.child == I);
        L2TlbRsToC r <- toGet(l2.rsToC).get;
        i.rsFromP.enq(ITlbRsFromP {entry: r.entry});
    endrule

    mkConnection(l2.setDirtyRs, d.setDirtyRsFromP);

    mkConnection(d.flush.request, l2.dTlbReqFlush);
    mkConnection(i.flush.request, l2.iTlbReqFlush);

    rule sendFlushDone;
        let x <- l2.flushDone.get;
        d.flush.response.put(?);
        i.flush.response.put(?);
    endrule
endmodule
