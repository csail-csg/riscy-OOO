import Types::*;
import ProcTypes::*;
import MMIOAddrs::*;
import CacheUtils::*;

// MMIO logic at platform (MMIOPlatform)
// XXX Currently all MMIO requests and posts of timer interrupts are handled
// one by one in a blocking manner. This is extremely conservative. Hopefully
// this may help avoid some kernel-level problems.

interface MMIOPlatform;
    method Action bootRomInitReq(BootRomIndex index, Data data);
    method ActionValue#(void) bootRomInitResp;
    method Action start(Addr toHostAddr, Addr fromHostAddr);
    interface FifoDeq#(Data) toHost;
    interface FifoEnq#(Data) fromHost;
endinterface

typedef enum {
    Init,
    SelectReq,
    ProcessReq,
    WaitResp
} MMIOPlatformState deriving(Bits, Eq, FShow);

// MMIO device/reg targed by the core request together with offset within
// reg/device
typedef enum {
    void TimerInterrupt, // auto-generated timer interrupt
    BootRomIndex BootRom,
    MSIPDataAlignedOffset MSIP,
    MTimCmpDataAlignedOffset MTimeCmp,
    void MTime,
    void ToHost,
    void FromHost,
    void Invalid // invalid req target
} MMIOPlatformReq deriving(Bits, Eq, FShow);

module mkMMIOPlatform#(Vector#(CoreNum, MMIOCoreToPlatform) cores)(
    MMIOPlatform
) provisos(
    Bits#(Data, 64) // this module assumes Data is 64-bit wide
);
    // boot rom
    BRAM_PORT_BE#(BootRomIndex, Data) bootRom <- mkBRAMCore1BE(
        valueOf(TExp#(LgBootRomSzData)), False
    );
    // mtimecmp
    Vector#(CoreNum, Reg#(Data)) mtimecmp <- replicateM(mkReg(0));
    // mtime
    Reg#(Data) mtime <- mkReg(0);
    // HTIF mem mapped addrs
    Fifo#(1, Data) toHostQ <- mkCFFifo;
    Fifo#(1, Data) fromHostQ <- mkCFFifo;
    Reg#(DataAlignedAddr) toHostAddr <- mkReg(0);
    Reg#(DataAlignedAddr) fromHostAddr <- mkReg(0);

    // state machine
    Reg#(MMIOPlatformState) state <- mkReg(Init);
    // current req (valid when state != Init && state != SelectReq
    Reg#(MMIOPlatformReq) curReq <- mkRegU;
    Reg#(CoreId) reqCore <- mkRegU;
    Reg#(Bool) isWrite <- mkRegU;
    Reg#(ByteEn) byteEn <- mkRegU;
    Reg#(Data) wrData <- mkRegU;
    // we need to wait for resp from cores when we need to change MTIP
    Reg#(Vector#(CoreNum, Bool)) waitMTIPCRs <- mkRegU;
    // for MSIP access: lower bits and upper bits of requested memory location
    // correspond to two cores. We need to wait resp from these two cores.
    Reg#(Maybe#(CoreId)) waitLowerMSIPCRs <- mkRegU;
    Reg#(Maybe#(CoreId)) waitUpperMSIPCRs <- mkRegU;

    // we increment mtime periodically
    Reg#(Bit#(TLog#(CyclesPerTimeInc))) cycle <- mkReg(0);

    // To avoid posting timer interrupt repeatedly, we keep a copy of MTIP
    // here. Since each core cannot write MTIP by CSRXXX inst, the only way to
    // change MTIP is through here.
    Vector#(CoreNum, Reg#(Bool)) mtip <- replicateM(mkReg(False));

    rule incCycle(
        state != Init &&
        cycle < fromInteger(valueof(CyclesPerTimeInc) - 1)
    );
        cycle <= cycle + 1;
    endrule

    // we don't increment mtime when processing a req
    rule incTime(
        state == SelectReq &&
        cycle >= fromInteger(valueof(CyclesPerTimeInc) - 1)
    );
        cycle <= 0;
        mtime <= mtime + fromInteger(valueof(TicksPerTimeInc));
    endrule

    // since we only process 1 MMIO req or timer interrupt at a time, we can
    // enq/deq all FIFOs in one rule

    (* preempts = "incTime, selectReq" *)
    rule selectReq(state == SelectReq);
        // check for timer interrupt
        Vector#(CoreNum, Bool) needTimerInt = replicate(False);
        for(Integer i = 0; i < valueof(CoreNum); i = i+1) begin
            if(!mtip[i] && mtimecmp[i] <= mtime) begin
                cores[i].pRq.enq(MMIOPRq {
                    target: MTIP,
                    write: True,
                    data: 1
                });
                mtip[i] <= True;
                needTimerInt[i] = True;
            end
        end
        if(needTimerInt != replicate(False)) begin
            state <= WaitResp;
            curReq <= TimerInterrupt;
            waitMTIPCRs <= needTimerInt;
        end
        else begin
            // now check for MMIO req from core
            function Bool hasReq(Integer i) = cores[i].cRq.notEmpty;
            Vector#(CoreNum, Integer) idxVec = genVector;
            if(find(hasReq, idxVec) matches tagged Valid .i) begin
                cores[i].cRq.deq;
                MMIOCRq req = cores[i].cRq.first;
                state <= ProcessReq;
                reqCore <= fromInteger(i);
                isWrite <= req.write;
                byteEn <= req.byteEn;
                wrData <= req.data;
                // find out which MMIO reg/device is being requested
                DataAlignedAddr addr = getDataAlignedAddr(req.addr);
                if(addr >= bootRomBaseAddr && req.addr < msipBoundAddr) begin
                    curReq <= BootRom (truncate(addr - bootRomBaseAddr));
                end
                else if(addr >= msipBaseAddr && req.addr < msipBoundAddr) begin
                    curReq <= MSIP (truncate(addr - msipBaseAddr));
                end
                else if(addr >= mtimecmpBaseAddr &&
                        req.addr < mtimecmpBoundAddr) begin
                    curReq <= MTimeCmp (truncate(addr - mtimecmpBaseAddr));
                end
                else if(addr == mtimeBaseAddr) begin
                    // assume mtime is of size Data
                    cureq <= MTime;
                end
                else if(addr == toHostAddr) begin
                    // assume tohost is of size Data
                    curReq <= ToHost;
                end
                else if(addr == fromHostAddr) begin
                    // assume fromhost is of size Data
                    curReq <= FromHost;
                end
                else begin
                    curReq <= Invalid;
                    assert(False, "Invalid MMIO req addr");
                end
            end
        end
    endrule

    // handle new timer interrupt: wait for writes on MTIP to be done
    rule waitTimerInterruptDone(state == WaitResp && curReq == TimerInterrupt);
        for(Integer i = 0; i < valueof(CoreNum); i = i+1) begin
            if(waitMTIPCRs[i]) begin
                cores[i].cRs.deq;
            end
        end
        state <= SelectCRq;
    endrule

    // handle boot rom access
    rule processBootRom(
        curReq matches tagged BootRom .offset &&& state == ProcessReq
    );
        if(isWrite) begin
            bootRom.put(pack(byteEn), offset, wrData);
            state <= SelectReq;
            cores[reqCore].pRs.enq(MMIOPRs {valid: True, data: ?});
        end
        else begin
            bootRom.put(0, offset, ?);
            state <= WaitResp;
        end
    endrule

    rule waitBootRom(
        curReq matches tagged BootRom .offset &&& state == WaitResp
    );
        state <= SelectReq;
        cores[reqCore].pRs.enq(MMIOPRs {
            valid: True,
            data: bootRom.read
        });
    endrule

    // handle MSIP access
    rule processMSIP(
        curReq matches tagged MSIP .offset &&& state == ProcessReq
    );
        // core corresponding to lower bits of requested Data
        CoreId lower_core = truncate({offset, 1'b0});
        Bool lower_en = byteEn[0];
        Bit#(1) lower_data = wrData[0];
        // core corresponding to upper bits of requested Data. Need to check if
        // this core truly exists
        CoreId upper_core = truncate({offset, 1'b1});
        Bool upper_valid = {offset, 1'b1} < fromInteger(valueof(CoreNum));
        Bool upper_en = byteEn[4];
        Bit#(1) upper_data = wrData[32];

        if(upper_en && !upper_valid) begin
            // access invalid core's MSIP, fault
            state <= SelectReq;
            cores[reqCore].pRs.enq(MMIOPRs {valid: False, data: ?});
        end
        else begin
            if(lower_en) begin
                cores[lower_core].pRq.enq(MMIOPRq {
                    target: MSIP,
                    write: isWrite,
                    data: lower_data
                });
            end
            if(upper_en) begin
                cores[upper_core].pRq.enq(MMIOPRq {
                    target: MSIP,
                    write: isWrite,
                    data: upper_data
                });
            end
            state <= WaitResp;
            waitLowerMSIPCRs <= lower_en ? Valid (lower_core) : Invalid;
            waitUpperMSIPCRs <= upper_en ? Valid (upper_core) : Invalid;
        end
    endrule

    rule waitMSIPDone(
        curReq matches tagged MSIP .offset &&& state == WaitResp
    );
        Bit#(32) lower_data = 0;
        if(waitLowerMSIPCRs matches tagged Valid .c) begin
            cores[c].cRs.deq;
            lower_data = zeroExtend(cores[c].cRs.first.data);
        end
        Bit#(32) upper_data = 0;
        if(waitUpperMSIPCRs matches tagged Valid .c) begin
            cores[c].cRs.deq;
            upper_data = zeroExtend(cores[c].cRs.first.data);
        end
        state <= SelectReq;
        cores[reqCore].pRs.enq(MMIOPRs {
            valid: True,
            data: {upper_data, lower_data}
        });
    endrule

    function Data getWriteData(Data orig);
        Vector#(NumBytes, Bit#(8)) data = unpack(orig);
        Vector#(NumBytes, Bit#(8)) wrVec = unpack(wrData);
        for(Integer i = 0; i < valueof(NumBytes); i = i+1) begin
            if(byteEn[i]) begin
                data[i] = wrVec[i];
            end
        end
        return pack(data);
    endfunction

    // handle mtimecmp access
    rule processMTimeCmp(
        curReq matches tagged MTimeCmp .offset &&& state == ProcessReq
    );
        if(offset <= fromInteger(valueof(CoreNum) - 1)) begin
            // access invalid core's mtimecmp, fault
            cores[reqCore].pRs.enq(MMIOPRs {valid: False, data: ?});
            state <= SelectReq;
        end
        else begin
            if(isWrite) begin
                // do write
                let newData = getWriteData(mtimecmp[offset]);
                mtimecmp[offset] <= newData;
                // check changes to MTIP
                if(newData <= mtime && !mtip[offset]]) begin
                    // need to post new timer interrupt
                    mtip[offset] <= True;
                    cores[offset].pRq.enq(MMIOPRq {
                        target: MTIP,
                        write: True,
                        data: 1
                    });
                    state <= WaitResp;
                end
                else if(newData > mtime && mtip[offset]) begin
                    // need to clear timer interrupt
                    mtip[offset] <= False;
                    cores[offset].pRq.enq(MMIOPRq {
                        target: MTIP,
                        write: True,
                        data: 0
                    });
                    state <= WaitResp;
                end
                else begin
                    // nothing happens to mtip, just finish this req
                    cores[reqCore].pRs.enq(MMIOPRs {valid: True, data: ?});
                    state <= SelectReq;
                end
            end
            else begin
                cores[reqCore].pRs.enq(MMIOPRs {
                    valid: True,
                    data: mtimecmp[offset]
                });
                state <= SelectReq;
            end
        end
    endrule

    rule waitMTimeCmpDone(
        curReq matches tagged MTimeCmp .offset &&& state == WaitResp
    );
        cores[offset].cRs.deq;
        cores[reqCore].pRs.enq(MMIOPRs {valid: True, data: ?});
        state <= SelectReq;
    endrule

    // handle mtime access
    rule processMTime(state == ProcessReq && curReq == MTime);
        if(isWrite) begin
            // do write
            let newData = getWriteData(mtime);
            mtime <= newData;
            // check change in MTIP
            Vector#(CoreNum, Bool) changeMTIP = replicate(False);
            for(Integer i = 0; i < valueof(CoreNum); i = i+1) begin
                if(mtimecmp[i] <= newData && !mtip[i]) begin
                    cores[i].pRq.enq(MMIOPRq {
                        target: MTIP,
                        write: True,
                        data: 1
                    });
                    changeMTIP[i] = True;
                end
                else if(mtimecmp[i] > newData && mtip[i]) begin
                    cores[i].pRq.enq(MMIOPRq {
                        target: MTIP,
                        write: True,
                        data: 0
                    });
                    changeMTIP[i] = True;
                end
            end
            if(changeMTIP != replicate(False)) begin
                waitMTIPCRs <= changeMTIP;
                state <= WaitResp;
            end
            else begin
                cores[reqCore].pRs.enq(MMIOPRs {valid: True, data: ?});
                state <= SelectReq;
            end
        end
        else begin
            cores[reqCore].pRs.enq(MMIOPRs {valid: True, data: mtime});
            state <= SelectReq;
        end
    endrule

    rule waitMTimeDone(state == WaitResp && curReq == MTime);
        for(Integer i = 0; i < valueof(CoreNum); i = i+1) begin
            if(waitMTIPCRs[i]) begin
                cores[i].cRs.deq;
            end
        end
        cores.pRs.enq(MMIOPRs {valid: True, data: ?});
        state <= SelectReq;
    endrule

    // handle tohost access
    rule processToHost(state == ProcessReq && curReq == ToHost);
        let resp = MMIOPRs {valid: False, data: ?};
        if(isWrite) begin
            if(toHostQ.notEmpty) begin
                let data = getWriteData(0);
                if(data != 0) begin // 0 means nothing for tohost
                    toHostQ.enq(data);
                end
                resp.valid = True;
            end
            else begin
                assert(False, "should not write tohost when not zero");
            end
        end
        else begin
            resp.valid = True;
            if(toHostQ.notEmpty) begin
                resp.data = toHostQ.first;
            end
            else begin
                resp.data = 0;
            end
        end
        state <= SelectReq;
        cores[reqCore].pRs.enq(resp);
    endrule

    // handle fromhost access
    rule processFromHost(state == ProcessReq && curReq == FromHost);
        let resp = MMIOPRs {valid: False, data: ?};
        if(isWrite) begin
            if(fromHostQ.notEmpty) begin
                if(getWriteData(fromHostQ.first) == 0) begin
                    fromHostQ.deq;
                    resp.valid = True;
                end
                else begin
                    assert(False, "Can only write 0 to fromhost");
                end
            end
            else begin
                if(getWriteData(0) == 0) begin
                    resp.valid = True;
                end
                else begin
                    assert(False, "Can only write 0 to fromhost");
                end
            end
        end
        else begin
            resp.valid = True;
            if(fromHostQ.notEmpty) begin
                resp.data = fromHostQ.first;
            end
            else begin
                resp.data = 0;
            end
        end
        state <= SelectReq;
        cores[reqCore].pRs.enq(resp);
    endrule
endmodule
