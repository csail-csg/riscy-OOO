import BRAMCore::*;
import Vector::*;
import Fifo::*;
import Types::*;
import ProcTypes::*;
import MMIOAddrs::*;
import MMIOCore::*;
import CacheUtils::*;

// MMIO logic at platform (MMIOPlatform)
// XXX Currently all MMIO requests and posts of timer interrupts are handled
// one by one in a blocking manner. This is extremely conservative. Hopefully
// this may help avoid some kernel-level problems.

interface MMIOPlatform;
    method Action bootRomInitReq(BootRomIndex index, Data data);
    method ActionValue#(void) bootRomInitResp;
    method Action start(Addr toHost, Addr fromHost);
    method ActionValue#(Data) to_host;
    method Action from_host(Data x);
endinterface

typedef enum {
    Init,
    SelectReq,
    ProcessReq,
    WaitResp
} MMIOPlatformState deriving(Bits, Eq, FShow);

// MMIO device/reg targed by the core request together with offset within
// reg/device
typedef union tagged {
    void Invalid; // invalid req target
    void TimerInterrupt; // auto-generated timer interrupt
    BootRomIndex BootRom;
    MSIPDataAlignedOffset MSIP;
    MTimCmpDataAlignedOffset MTimeCmp;
    void MTime;
    void ToHost;
    void FromHost;
} MMIOPlatformReq deriving(Bits, Eq, FShow);

module mkMMIOPlatform#(Vector#(CoreNum, MMIOCoreToPlatform) cores)(
    MMIOPlatform
) provisos(
    Bits#(Data, 64) // this module assumes Data is 64-bit wide
);
    Bool verbose = True;

    // boot rom
    BRAM_PORT_BE#(BootRomIndex, Data, NumBytes) bootRom <- mkBRAMCore1BE(
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

    // respQ for boot rom init
    Fifo#(1, void) bootRomInitRespQ <- mkCFFifo;

    // pass mtime to each core
    rule propagateTime(state != Init);
        for(Integer i = 0; i < valueof(CoreNum); i = i+1) begin
            cores[i].setTime(mtime);
        end
    endrule

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
            if(verbose) begin
                $display("[Platform - SelectReq] timer interrupt",
                         ", mtime %x", mtime,
                         ", mtimcmp ", fshow(readVReg(mtimecmp)),
                         ", old mtip ", fshow(readVReg(mtip)),
                         ", new interrupts ", fshow(needTimerInt));
            end
        end
        else begin
            // now check for MMIO req from core
            function Bool hasReq(Integer i) = cores[i].cRq.notEmpty;
            Vector#(CoreNum, Integer) idxVec = genVector;
            if(find(hasReq, idxVec) matches tagged Valid .i) begin
                cores[i].cRq.deq;
                MMIOCRq req = cores[i].cRq.first;
                // record req
                reqCore <= fromInteger(i);
                isWrite <= req.write;
                byteEn <= req.byteEn;
                wrData <= req.data;
                // find out which MMIO reg/device is being requested
                DataAlignedAddr addr = getDataAlignedAddr(req.addr);
                MMIOPlatformReq newReq = Invalid;
                if(addr >= bootRomBaseAddr && addr < bootRomBoundAddr) begin
                    newReq = BootRom (truncate(addr - bootRomBaseAddr));
                end
                else if(addr >= msipBaseAddr && addr < msipBoundAddr) begin
                    newReq = MSIP (truncate(addr - msipBaseAddr));
                end
                else if(addr >= mtimecmpBaseAddr &&
                        addr < mtimecmpBoundAddr) begin
                    newReq = MTimeCmp (truncate(addr - mtimecmpBaseAddr));
                end
                else if(addr == mtimeBaseAddr) begin
                    // assume mtime is of size Data
                    newReq = MTime;
                end
                else if(addr == toHostAddr) begin
                    // assume tohost is of size Data
                    newReq = ToHost;
                end
                else if(addr == fromHostAddr) begin
                    // assume fromhost is of size Data
                    newReq = FromHost;
                end
                if(newReq != Invalid) begin
                    // process valid req
                    curReq <= newReq;
                    state <= ProcessReq;
                end
                else begin
                    // access fault
                    cores[i].pRs.enq(MMIOPRs {valid: False, data: ?});
                end
                if(verbose) begin
                    $display("[Platform - SelectReq] new req, core %d, req ",
                             i, fshow(req), ", type ", fshow(newReq));
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
        state <= SelectReq;
        if(verbose) begin
            $display("[Platform - Done] timer interrupt",
                     ", mtip ", fshow(readVReg(mtip)),
                     ", waitCRs ", fshow(waitMTIPCRs));
        end
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
        if(verbose) begin
            $display("[Platform - boot rom done], core %d, data %x",
                     reqCore, bootRom.read);
        end
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
            if(verbose) begin
                $display("[Platform - process msip] access fault");
            end
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
        Bit#(32) upper_data = 0;
        for(Integer i = 0; i < valueof(CoreNum); i = i+1) begin
            if (waitLowerMSIPCRs matches tagged Valid .c &&&
                c == fromInteger(i)) begin
                cores[i].cRs.deq;
                lower_data = zeroExtend(cores[i].cRs.first.data);
            end
            else if(waitUpperMSIPCRs matches tagged Valid .c &&&
                    c == fromInteger(i)) begin
                cores[i].cRs.deq;
                upper_data = zeroExtend(cores[i].cRs.first.data);
            end
        end
        state <= SelectReq;
        cores[reqCore].pRs.enq(MMIOPRs {
            valid: True,
            data: {upper_data, lower_data}
        });
        if(verbose) begin
            $display("[Platform - msip done] lower %x, upper %x",
                     lower_data, upper_data);
        end
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
        if(offset > fromInteger(valueof(CoreNum) - 1)) begin
            // access invalid core's mtimecmp, fault
            cores[reqCore].pRs.enq(MMIOPRs {valid: False, data: ?});
            state <= SelectReq;
            if(verbose) begin
                $display("[Platform - process mtimecmp] access fault");
            end
        end
        else begin
            if(isWrite) begin
                // do write
                let newData = getWriteData(mtimecmp[offset]);
                mtimecmp[offset] <= newData;
                // check changes to MTIP
                if(newData <= mtime && !mtip[offset]) begin
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
                    if(verbose) begin
                        $display("[Platform - process mtimecmp] ",
                                 "no change to mtip ", fshow(readVReg(mtip)),
                                 ", mtime %x", mtime,
                                 ", old mtimecmp ", fshow(readVReg(mtimecmp)),
                                 ", new mtimecmp[%d] %x", offset, newData);
                    end
                end
            end
            else begin
                cores[reqCore].pRs.enq(MMIOPRs {
                    valid: True,
                    data: mtimecmp[offset]
                });
                state <= SelectReq;
                if(verbose) begin
                    $display("[Platform - process mtimecmp] read done, data %x",
                             mtimecmp[offset]);
                end
            end
        end
    endrule

    rule waitMTimeCmpDone(
        curReq matches tagged MTimeCmp .offset &&& state == WaitResp
    );
        cores[offset].cRs.deq;
        cores[reqCore].pRs.enq(MMIOPRs {valid: True, data: ?});
        state <= SelectReq;
        if(verbose) begin
            $display("[Platform - mtimecmp done]",
                     ", mtime %x", mtime,
                     ", mtimecmp ", fshow(readVReg(mtimecmp)),
                     ", mtip ", fshow(readVReg(mtip)));
        end
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
                if(verbose) begin
                    $display("[Platform - process mtime] ",
                             "no change to mtip ", fshow(readVReg(mtip)),
                             ", new mtime %x", newData,
                             ", mtimecmp ", fshow(readVReg(mtimecmp)));
                end
            end
        end
        else begin
            cores[reqCore].pRs.enq(MMIOPRs {valid: True, data: mtime});
            state <= SelectReq;
            if(verbose) begin
                $display("[Platform - process mtime] read done, data %x",
                         mtime);
            end
        end
    endrule

    rule waitMTimeDone(state == WaitResp && curReq == MTime);
        for(Integer i = 0; i < valueof(CoreNum); i = i+1) begin
            if(waitMTIPCRs[i]) begin
                cores[i].cRs.deq;
            end
        end
        cores[reqCore].pRs.enq(MMIOPRs {valid: True, data: ?});
        state <= SelectReq;
        if(verbose) begin
            $display("[Platform - mtime done]",
                     ", mtime %x", mtime,
                     ", mtimecmp ", fshow(readVReg(mtimecmp)),
                     ", mtip ", fshow(readVReg(mtip)));
        end
    endrule

    // handle tohost access
    rule processToHost(state == ProcessReq && curReq == ToHost);
        let resp = MMIOPRs {valid: False, data: ?};
        if(isWrite) begin
            if(toHostQ.notEmpty) begin
                doAssert(False, "Cannot write tohost when toHostQ not empty");
                // this will raise access fault
            end
            else begin
                let data = getWriteData(0);
                if(data != 0) begin // 0 means nothing for tohost
                    toHostQ.enq(data);
                end
                resp.valid = True;
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
        if(verbose) begin
            $display("[Platform - process tohost] resp ", fshow(resp));
        end
    endrule

    rule writeToHostStuck(state == ProcessReq && curReq == ToHost &&
                       isWrite && toHostQ.notEmpty);
        $display("[Platform - process tohost] WARNING: ",
                 "write when toHostQ not empty");
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
                    doAssert(False, "Can only write 0 to fromhost");
                end
            end
            else begin
                if(getWriteData(0) == 0) begin
                    resp.valid = True;
                end
                else begin
                    doAssert(False, "Can only write 0 to fromhost");
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
        if(verbose) begin
            $display("[Platform - process fromhost] resp ", fshow(resp));
        end
    endrule

    method Action bootRomInitReq(BootRomIndex idx, Data data) if(state == Init);
        bootRom.put(maxBound, idx, data);
        bootRomInitRespQ.enq(?);
    endmethod

    method ActionValue#(void) bootRomInitResp;
        bootRomInitRespQ.deq;
        return ?;
    endmethod

    method Action start(Addr toHost, Addr fromHost) if(state == Init);
        toHostAddr <= getDataAlignedAddr(toHost);
        fromHostAddr <= getDataAlignedAddr(fromHost);
        state <= SelectReq;
    endmethod

    method ActionValue#(Data) to_host;
        toHostQ.deq;
        return toHostQ.first;
    endmethod
    method Action from_host(Data x);
        fromHostQ.enq(x);
    endmethod
endmodule
