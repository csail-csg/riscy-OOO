import Clocks::*;
import Connectable::*;
import BRAMCore::*;
import Types::*;
import ProcTypes::*;
import Fifo::*;
import BootRomIF::*;
import SyncFifo::*;

interface BootRomMMIO;
    method Action req(BootRomIndex index);
    method ActionValue#(Data) resp;
endinterface

interface BootRomIndInv;
    method Action initDone;
endinterface

instance Connectable#(BootRomIndInv, BootRomIndication);
    module mkConnection#(BootRomIndInv inv, BootRomIndication ind)(Empty);
        rule doInitDone;
            inv.initDone;
            ind.initDone;
        endrule
    endmodule
endinstance

interface BootRom;
    interface BootRomMMIO mmio;
    interface BootRomRequest hostReq;
    interface BootRomIndInv hostIndInv;
endinterface

typedef struct {
    BootRomIndex index;
    Data data;
    Bool last;
} BootRomInit deriving(Bits, Eq, FShow);

(* synthesize *)
module mkBootRom#(Clock portalClk, Reset portalRst)(BootRom);
    Reg#(Bool) inited <- mkReg(False);

    BRAM_PORT#(BootRomIndex, Data) bootRom <- mkBRAMCore1(
        valueOf(TExp#(LgBootRomSzData)), False
    );

    // 1 element fifo to hold mmio req
    Fifo#(1, void) mmioReqQ <- mkPipelineFifo;

    // sync FIFOs to cross to portal clk
    Clock userClk <- exposeCurrentClock;
    Reset userRst <- exposeCurrentReset;
    SyncFIFOIfc#(BootRomInit) hostInitReqQ <- mkSyncFifo(1, portalClk, portalRst, userClk, userRst);
    SyncFIFOIfc#(void) hostInitDoneQ <- mkSyncFifo(1, userClk, userRst, portalClk, portalRst);

    rule doInit(!inited);
        hostInitReqQ.deq;
        let r = hostInitReqQ.first;
        bootRom.put(True, r.index, r.data);
        // check init done
        if(r.last) begin
            inited <= True;
            hostInitDoneQ.enq(?);
        end
    endrule

    interface BootRomMMIO mmio;
        method Action req(BootRomIndex index) if(inited);
            bootRom.put(False, index, ?);
            mmioReqQ.enq(?);
        endmethod
        method ActionValue#(Data) resp if(inited);
            mmioReqQ.deq;
            return bootRom.read;
        endmethod
    endinterface

    interface BootRomRequest hostReq;
        method Action initReq(Bit#(16) index, Bit#(64) v, Bool last);
            hostInitReqQ.enq(BootRomInit {
                index: truncate(index),
                data: v,
                last: last
            });
        endmethod
    endinterface

    interface BootRomIndInv hostIndInv;
        method Action initDone;
            hostInitDoneQ.deq;
        endmethod
    endinterface
endmodule
