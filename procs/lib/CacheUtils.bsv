`include "ProcConfig.bsv"
import BRAM::*;
import Types::*;
import MemoryTypes::*;
import GetPut::*;
import ClientServer::*;
import Connectable::*;
import Vector::*;
import Fifo::*;
import Ehr::*;
import FIFO::*;
import FIFOF::*;
import Axi4MasterSlave::*;
import Axi4Interconnect::*;
import Performance::*;
import FShow::*;
import MsgFifo::*;

// XXX: move to MemoryTypes.bsv
typedef Bit#(RiscyAxiBusWidth) RiscyAxiData;
typedef TDiv#(RiscyAxiBusWidth, 8) RiscyAxiNumBytes;

function Bool isRiscyAxiAlignAddr(Addr a);
    Bit#(TLog#(RiscyAxiNumBytes)) offset = truncate(a);
    return offset == 0;
endfunction

// 64B cache line
typedef 8 CLineNumData;
typedef TLog#(CLineNumData) LogCLineNumData;
typedef Bit#(LogCLineNumData) CLineDataSel;

function CLineDataSel getCLineDataSel(Addr a);
    return truncate(a >> valueOf(TLog#(NumBytes)));
endfunction

typedef TMul#(CLineNumData, DataSz) CacheLineSz;
typedef Bit#(CacheLineSz) CacheLine;

typedef TDiv#(CacheLineSz, WideLineSz) CLineNumWide;
typedef TLog#(CLineNumWide) LogCLineNumWide;

typedef TMul#(CLineNumData, NumBytes) CLineNumBytes;
typedef TLog#(CLineNumBytes) LogCLineNumBytes;
typedef Vector#(CLineNumBytes, Bool) CLineByteEn;

function Bool isCLineAlignAddr(Addr a);
    Bit#(LogCLineNumBytes) offset = truncate(a);
    return offset == 0;
endfunction

// cache line addr (drop the offset within cache line)
typedef TSub#(AddrSz, LogCLineNumBytes) CLineAddrSz;
typedef Bit#(CLineAddrSz) CLineAddr;

// cache line v.s. instruction
typedef TDiv#(CacheLineSz, InstSz) CLineNumInst;
typedef Bit#(TLog#(CLineNumInst)) CLineInstSel;

function CLineInstSel getCLineInstSel(Addr a);
    return truncate(a >> valueof(TLog#(TDiv#(InstSz, 8))));
endfunction

// pending (write) response count
typedef Bit#(5) PendMemRespCnt;

interface Cache;
    method Action flush; // invalidate whole cache
    method Bool flush_done;
    interface Server#(MemReq, MemResp) to_proc;
    interface RiscyAxiMaster to_mem;
    interface Perf#(L1PerfType) perf;
endinterface

// BRAM with 1 write port and 1 read port
interface RWBram#(type addrT, type dataT);
    method Action wrReq(addrT a, dataT d);
    method Action rdReq(addrT a);
    method ActionValue#(dataT) rdResp;
endinterface

module mkRWBram(RWBram#(addrT, dataT)) provisos(
    Bits#(addrT, a__), Bits#(dataT, b__));

    BRAM_Configure cfg = defaultValue;
    BRAM2Port#(addrT, dataT) ram <- mkBRAM2Server(cfg);

    // port A: read
    // port B: write

    method Action wrReq(addrT a, dataT d);
        ram.portB.request.put(BRAMRequest {
            write: True,
            responseOnWrite: False,
            address: a,
            datain: d
        });
    endmethod

    method Action rdReq(addrT a);
        ram.portA.request.put(BRAMRequest {
            write: False,
            responseOnWrite: False,
            address: a,
            datain: ?
        });
    endmethod

    method ActionValue#(dataT) rdResp;
        let d <- ram.portA.response.get;
        return d;
    endmethod
endmodule

// cache (load) req to mem
typedef struct {
    CLineAddr cAddr;
    idT id;
} CacheReqToMem#(type idT) deriving(Bits, Eq, FShow);

// cache (write-back) resp to mem
typedef struct {
    CLineAddr cAddr;
    CacheLine data;
} CacheRespToMem deriving(Bits, Eq, FShow);

// (load) resp from mem
typedef struct {
    CacheLine data;
    idT id;
} MemRespToCache#(type idT) deriving(Bits, Eq, FShow);

// write buffer between cache and AXI
// assume AXI keeps St-->St ordering
// this can help keep St-->Ld (same addr) ordering
interface WriteBuffer#(numeric type sz);
    // bypass data from the youngest matching St req
    method Maybe#(CacheLine) bypass(CLineAddr cAddr);
    method Bool notFull;
    method Action enq(CLineAddr a, CacheLine d);
    method Bool notEmpty;
    method Action deq;
endinterface

module mkCFWriteBuffer(WriteBuffer#(buffSz)) provisos (
    Add#(1, a__, buffSz),
    Alias#(elemCnt, Bit#(TLog#(TAdd#(buffSz, 1))))
);
    // youngest req is always in addr/dataVec[0]
    Vector#(buffSz, Reg#(CLineAddr)) addrVec <- replicateM(mkRegU);
    Vector#(buffSz, Reg#(CacheLine)) dataVec <- replicateM(mkRegU);
    Reg#(elemCnt) cnt <- mkReg(0); // number of valid req

    Ehr#(2, Maybe#(Tuple2#(CLineAddr, CacheLine))) enqReq <- mkEhr(Invalid);
    Ehr#(2, Bool) deqReq <- mkEhr(False);

    (* fire_when_enabled, no_implicit_conditions *)
    rule cononicalize;
        // insert new req into position 0
        // and shift older req backwards
        if(enqReq[1] matches tagged Valid {.cAddr, .data}) begin
            for(Integer i = 1; i < valueOf(buffSz); i = i+1) begin
                addrVec[i] <= addrVec[i - 1];
                dataVec[i] <= dataVec[i - 1];
            end
            addrVec[0] <= cAddr;
            dataVec[0] <= data;
        end
        // change valid req number
        elemCnt cntNext = cnt;
        if(isValid(enqReq[1])) begin
            cntNext = cntNext + 1;
        end
        if(deqReq[1]) begin
            cntNext = cntNext - 1;
        end
        cnt <= cntNext;
        // reset EHRs
        enqReq[1] <= Invalid;
        deqReq[1] <= False;
    endrule

    method Maybe#(CacheLine) bypass(CLineAddr cAddr);
        // bypass from YOUNGEST
        Maybe#(CacheLine) hit = Invalid;
        for(Integer i = 0; i < valueOf(buffSz); i = i+1) begin
            if(!isValid(hit) && fromInteger(i) < cnt && addrVec[i] == cAddr) begin
                hit = Valid (dataVec[i]);
            end
        end
        return hit;
    endmethod

    method Bool notFull = cnt < fromInteger(valueOf(buffSz));

    method Action enq(CLineAddr a, CacheLine d) if(cnt < fromInteger(valueOf(buffSz)));
        enqReq[0] <= Valid (tuple2(a, d));
    endmethod

    method Bool notEmpty = cnt > 0;

    method Action deq if(cnt > 0);
        deqReq[0] <= True;
    endmethod
endmodule

// relation between cache line and axi bus
typedef TDiv#(CacheLineSz, RiscyAxiBusWidth) CLineNumAxi;
typedef TLog#(CLineNumAxi) LogCLineNumAxi;
typedef Bit#(LogCLineNumAxi) CLineAxiSel;

function CLineAxiSel getCLineAxiSel(Addr a); // used in test bench
    return truncate(a >> valueOf(TLog#(RiscyAxiNumBytes)));
endfunction

interface FifoEnq#(type t);
    method Bool notFull;
    method Action enq(t x);
endinterface

function FifoEnq#(t) toFifoEnq(Fifo#(n, t) f);
    return (interface FifoEnq;
        method notFull = f.notFull;
        method enq = f.enq;
    endinterface);
endfunction

function FifoEnq#(t) nullFifoEnq;
    return (interface FifoEnq;
        method Bool notFull = True;
        method Action enq(t x);
            noAction;
        endmethod
    endinterface);
endfunction

instance ToPut#(FifoEnq#(t), t);
    function Put#(t) toPut(FifoEnq#(t) ifc);
        return (interface Put;
            method Action put(t x);
                ifc.enq(x);
            endmethod
        endinterface);
    endfunction
endinstance

interface FifoDeq#(type t);
    method Bool notEmpty;
    method Action deq;
    method t first;
endinterface

function FifoDeq#(t) toFifoDeq(Fifo#(n, t) f);
    return (interface FifoDeq;
        method notEmpty = f.notEmpty;
        method deq = f.deq;
        method first = f.first;
    endinterface);
endfunction

function FifoDeq#(t) nullFifoDeq;
    return (interface FifoDeq;
        method Bool notEmpty = False;
        method Action deq if(False);
            noAction;
        endmethod
        method t first if(False);
            return ?;
        endmethod
    endinterface);
endfunction

instance ToGet#(FifoDeq#(t), t);
    function Get#(t) toGet(FifoDeq#(t) ifc);
        return (interface Get;
            method ActionValue#(t) get;
                ifc.deq;
                return ifc.first;
            endmethod
        endinterface);
    endfunction
endinstance

instance Connectable#(FifoEnq#(t), FifoDeq#(t));
    module mkConnection#(FifoEnq#(t) enq, FifoDeq#(t) deq)(Empty);
        mkConnection(toGet(deq), toPut(enq));
    endmodule
endinstance

instance Connectable#(FifoDeq#(t), FifoEnq#(t));
    module mkConnection#(FifoDeq#(t) deq, FifoEnq#(t) enq)(Empty);
        mkConnection(toGet(deq), toPut(enq));
    endmodule
endinstance

// connect cache to AXI
// load req can never block wb resp
interface CacheToAxi#(numeric type writeBufferSz, numeric type ldQSz, type idT);
    interface MsgFifoEnq#(CacheReqToMem#(idT), CacheRespToMem) msgToAxi;
    interface FifoDeq#(MemRespToCache#(idT)) ldResp;
    interface FifoDeq#(void) wbDone;
    interface RiscyAxiMaster to_mem;
endinterface

// AXI should keep St-->St and Ld-->Ld orderings
// this module additionally enforces St-->Ld (same addr) ordering
// by assuming that no other is writing memory (NO other writer)
// NOTE: Ld-->St ordering is not kept here
// but it should be automatically enforced for same addr in the cache
// (we cannot evict a cache line when we get a miss on it)
module mkCacheToAxi(CacheToAxi#(writeBufferSz, ldQSz, idT)) provisos(
    Bits#(idT, idSz),
    Add#(1, a__, writeBufferSz)
);
    // FIFO to interface cache
    MsgFifo#(2, CacheReqToMem#(idT), CacheRespToMem) msgInQ <- mkMsgFifo;
    Fifo#(2, MemRespToCache#(idT)) ldRespQ <- mkCFFifo;
    Fifo#(2, void) wbDoneQ <- mkCFFifo;

    // write buffer
    WriteBuffer#(writeBufferSz) wrBuff <- mkCFWriteBuffer;

    // logic to interface AXI bus
    // req ID will be carried along with AXI req/resp
    RiscyAxiFIFOs axiFifos <- mkAxi4FIFOs;

    // Ld req/resp ID FIFO
    FIFO#(idT) ldIdQ <- mkSizedFIFO(valueOf(ldQSz));

    // for serialize wb resp (store)
    Reg#(CLineAddr) stAddr <- mkRegU;
    Ehr#(2, CacheLine) stData <- mkEhr(?);
    Ehr#(2, Bit#(TLog#(TAdd#(CLineNumAxi, 1)))) stRemainCnt <- mkEhr(0);
    // port 0: to axi
    Reg#(CacheLine) stData_axi = stData[0];
    Reg#(Bit#(TLog#(TAdd#(CLineNumAxi, 1)))) stRemainCnt_axi = stRemainCnt[0];
    // port 1: to cache
    Reg#(CacheLine) stData_cache = stData[1];
    Reg#(Bit#(TLog#(TAdd#(CLineNumAxi, 1)))) stRemainCnt_cache = stRemainCnt[1];

    // for deserialize load resp
    Reg#(Bit#(TSub#(CacheLineSz, RiscyAxiBusWidth))) respData <- mkRegU;
    Reg#(CLineAxiSel) respRecvCnt <- mkReg(0);

    // get a new wb resp from cache
    rule doWbRespFromCache(msgInQ.first matches tagged Resp .r &&& stRemainCnt_cache == 0);
        msgInQ.deq;
        // send store to write buffer
        wrBuff.enq(r.cAddr, r.data);
        // send store to AXI
        stAddr <= r.cAddr;
        stData_cache <= r.data;
        stRemainCnt_cache <= fromInteger(valueOf(CLineNumAxi));
    endrule

    // search write buffer for bypassing
    Maybe#(CacheLine) hitWrBuff = (case(msgInQ.first) matches
        tagged Req .r: wrBuff.bypass(r.cAddr);
        tagged Resp .r: Invalid;
    endcase);

    // get new ld req that hits in write buffer
    rule doLdReqFromCache_bypass(msgInQ.first matches tagged Req .r &&& isValid(hitWrBuff));
        msgInQ.deq;
        // directly respond data
        ldRespQ.enq(MemRespToCache {
            data: fromMaybe(?, hitWrBuff),
            id: r.id
        }); 
        //$display("[CacheToAxi] ", fshow(r), " bypass get %x", fromMaybe(?, hitWrBuff));
    endrule

    // get new ld req that miss in write buffer
    rule doLdReqFromCache_toMem(msgInQ.first matches tagged Req .r &&& !isValid(hitWrBuff));
        msgInQ.deq;
        // send read req to AXI
        RiscyAxiReadRequest axi_read_req = defaultValue;
        axi_read_req.address = {r.cAddr, 0};
        // len is 1 less than the number of transfers per burst
        axi_read_req.len = fromInteger(valueOf(CLineNumAxi) - 1);
        axiFifos.readReq.enq(axi_read_req);
        // save ID
        ldIdQ.enq(r.id);
    endrule

    // serialize store to AXI
    rule sendStReqToAxi(stRemainCnt_axi > 0);
        // send write req to AXI for the first transfer
        if(stRemainCnt_axi == fromInteger(valueOf(CLineNumAxi))) begin
            RiscyAxiWriteRequest axi_write_req = defaultValue;
            axi_write_req.address = {stAddr, 0};
            // len is 1 less than the number of transfers per burst
            axi_write_req.len = fromInteger(valueOf(CLineNumAxi) - 1);
            axiFifos.writeReq.enq(axi_write_req);
        end
        // always send write data
        RiscyAxiWriteData axi_write_data = defaultValue;
        axi_write_data.data = truncate(stData_axi);
        axi_write_data.byteEnable = maxBound;
        axi_write_data.last = stRemainCnt_axi == 1 ? 1 : 0;
        axiFifos.writeData.enq(axi_write_data);
        // change st req info
        stRemainCnt_axi <= stRemainCnt_axi - 1;
        stData_axi <= stData_axi >> valueOf(RiscyAxiBusWidth);
    endrule

    Bool recvLast = respRecvCnt == fromInteger(valueOf(CLineNumAxi) - 1);

    // deserialize ld resp from AXI
    // arbitrate between resp from mem, and resp from bypass
    // give priority to bypass
    (* descending_urgency = "doLdReqFromCache_bypass, recvLdRespFromAxi_last" *)
    rule recvLdRespFromAxi_last(recvLast);
        RiscyAxiReadResponse d = axiFifos.readResp.first;
        axiFifos.readResp.deq;
        if(d.last != 1) begin
            $fwrite(stderr, "[CacheToAxi] ERROR: readResp.lasti = %d is unexpected\n", d.last);
            $finish;
        end 
        // send resp to cache & remove ID from FIFO
        ldIdQ.deq;
        let id = ldIdQ.first;
        ldRespQ.enq(MemRespToCache {
            data: {d.data, respData},
            id: id
        });
        respRecvCnt <= 0;
    endrule

    rule recvLdRespFromAxi_notLast(!recvLast);
        RiscyAxiReadResponse d = axiFifos.readResp.first;
        axiFifos.readResp.deq;
        if(d.last != 0) begin
            $fwrite(stderr, "[CacheToAxi] ERROR: readResp.last = %d is unexpected\n", d.last);
            $finish;
        end 
        respRecvCnt <= respRecvCnt + 1;
        respData <= truncateLSB({d.data, respData});
    endrule

    rule recvStRespFromAxi;
        axiFifos.writeResp.deq;
        wrBuff.deq;
        wbDoneQ.enq(?); // send resp to cache
    endrule

    interface MsgFifoEnq msgToAxi = toMsgFifoEnq(msgInQ);
    interface FifoDeq ldResp = toFifoDeq(ldRespQ);
    interface FifoDeq wbDone = toFifoDeq(wbDoneQ);
    interface RiscyAxiMaster to_mem = toAxi4Master(axiFifos);
endmodule


// dummy one
module mkDummyCache (Cache ifc);
  let verbose = False;

  RiscyAxiFIFOs riscyAxiFifos <- mkAxi4FIFOs;

  rule ignoreWriteResps;
    riscyAxiFifos.writeResp.deq;
  endrule

  interface Server to_proc;
    interface Put request;
      method Action put(MemReq r);
        if (r.op == Ld) begin
          RiscyAxiReadRequest axi_read_req = defaultValue; // 8 byte read by default
          axi_read_req.address = r.addr;
          riscyAxiFifos.readReq.enq(axi_read_req);
        end else if (r.op == St) begin
          // format write request (address)
          RiscyAxiWriteRequest axi_write_req = defaultValue;
          axi_write_req.address = r.addr;
          riscyAxiFifos.writeReq.enq(axi_write_req);
          // format write data
          RiscyAxiWriteData axi_write_data = defaultValue;
          axi_write_data.data = r.data;
          axi_write_data.last = 1;
          riscyAxiFifos.writeData.enq(axi_write_data);
        end
      endmethod
    endinterface

    interface Get response;
      method ActionValue#(MemResp) get;
        let axi_read_resp = riscyAxiFifos.readResp.first;
        riscyAxiFifos.readResp.deq;
        let data = axi_read_resp.data;
        return data;
      endmethod
    endinterface
  endinterface

  interface RiscyAxiMaster to_mem = toAxi4Master(riscyAxiFifos);

  interface Perf perf;
    method Action setStatus(Bool stats);
      noAction;
    endmethod
    method Action req(L1PerfType r);
      noAction;
    endmethod
    method ActionValue#(PerfResp#(perfType)) resp if(False);
      return ?;
    endmethod
    method Bool respValid = False;
  endinterface

  method Action flush;
    noAction;
  endmethod

  method Bool flush_done;
    return True;
  endmethod
endmodule


