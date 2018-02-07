import Vector::*;
import ConfigReg::*;
import Fifo::*;
import Types::*;
import ProcTypes::*;
import CCTypes::*;
import CacheUtils::*;
import MMIOAddrs::*;

interface MMIOInstToCore;
    interface FifoDeq#(Tuple2#(Addr, SupWaySel)) instReq;
    interface FifoEnq#(Vector#(SupSize, Maybe#(Instruction))) instResp;
    method Action setHtifAddrs(Addr toHost, Addr fromHost);
endinterface

typedef enum {
    MainMem,
    BootRom,
    Fault
} InstFetchTarget deriving(Bits, Eq, FShow);

interface MMIOInst;
    // Figure out where to fetch the inst, i.e., from boot rom or main memory.
    method InstFetchTarget getFetchTarget(Addr phyPc);
    // When req boot rom, need to specify the number of instructions to fetch,
    // i.e., maxWay + 1
    method Action bootRomReq(Addr phyPc, SupWaySel maxWay);
    // The return type is same as I$. An entry is Invalid if it is an access
    // fault or not requested before.
    method ActionValue#(Vector#(SupSize, Maybe#(Instruction))) bootRomResp;
    interface MMIOInstToCore toCore;
endinterface

(* synthesize *)
module mkMMIOInst(MMIOInst);
    // record tohost/fromhost addrs to check inst access fault. Since this
    // MMIOInst module will be placed inside the fetch stage and we want to
    // synthesize fetch stage, we need to add these two regs instead of passing
    // in two reg interfaces.
    Reg#(DataAlignedAddr) toHostAddr <- mkConfigReg(0);
    Reg#(DataAlignedAddr) fromHostAddr <- mkConfigReg(0);
    // MMIO requests are handled in a very slow manner at platform, so no need
    // to use large FIFO here
    Fifo#(1, Tuple2#(Addr, SupWaySel)) reqQ <- mkCFFifo;
    Fifo#(1, Vector#(SupSize, Maybe#(Instruction))) respQ <- mkCFFifo;
    // To prevent inst fetch requests from clogging the network, we limit to at
    // most 1 pending req. The resp for the pending req will be buffered in
    // pRsQ, no affecting other MMIO accesses.
    Fifo#(1, void) pendQ <- mkCFFifo;

    method InstFetchTarget getFetchTarget(Addr phyPc);
        let addr = getDataAlignedAddr(phyPc);
        if(addr >= bootRomBaseAddr && addr < bootRomBoundAddr) begin
            return BootRom;
        end
        else if(addr >= mainMemBaseAddr &&
                addr != toHostAddr && addr != fromHostAddr) begin
            return MainMem;
        end
        else begin
            return Fault;
        end
    endmethod

    method Action bootRomReq(Addr phyPc, SupWaySel maxWay);
        reqQ.enq(tuple2(phyPc, maxWay));
        pendQ.enq(?);
    endmethod

    method ActionValue#(Vector#(SupSize, Maybe#(Instruction))) bootRomResp;
        pendQ.deq;
        respQ.deq;
        return respQ.first;
    endmethod

    interface MMIOInstToCore toCore;
        interface instReq = toFifoDeq(reqQ);
        interface instResp = toFifoEnq(respQ);
        method Action setHtifAddrs(Addr toHost, Addr fromHost);
            toHostAddr <= getDataAlignedAddr(toHost);
            fromHostAddr <= getDataAlignedAddr(fromHost);
        endmethod
    endinterface
endmodule
