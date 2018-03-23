
import Clocks::*;
import FIFOF::*;
import BRAMFIFO::*;

module mkSyncFifo#(
    Integer depth, Clock srcClk, Reset srcRst, Clock dstClk, Reset dstRst
)(SyncFIFOIfc#(t)) provisos(
    Bits#(t, tW),
    NumAlias#(w, TMax#(1, tW)) // some sync fifo doesn't support 0-bit data
);
    // this module should not be really used
    SyncFIFOIfc#(Bit#(w)) q <- mkSyncFIFO(depth, srcClk, srcRst, dstClk);

    method notFull = q.notFull;
    method Action enq(t x);
        q.enq(zeroExtend(pack(x)));
    endmethod

    method notEmpty = q.notEmpty;
    method t first;
        return unpack(truncate(q.first));
    endmethod
    method Action deq;
        q.deq;
    endmethod
endmodule

module mkSyncBramFifo#(
    Integer depth, Clock srcClk, Reset srcRst, Clock dstClk, Reset dstRst
)(SyncFIFOIfc#(t)) provisos(
    Bits#(t, tW),
    NumAlias#(w, TMax#(1, tW)) // some sync fifo doesn't support 0-bit data
);
    // this module should not be really used
    SyncFIFOIfc#(t) f <- mkSyncFifo(depth, srcClk, srcRst, dstClk, dstRst);
    return f;
endmodule
