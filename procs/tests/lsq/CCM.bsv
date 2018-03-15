import CacheUtils::*;
import Vector::*;
import Types::*;
import MemoryTypes::*;
import TestTypes::*;
import RegFile::*;
import Amo::*;
import MsgFifo::*;
import Assert::*;
import Ehr::*;
import Randomizable::*;
import SatDownCounter::*;

interface CCM;
    method Action procReq(CCMReqId id, MemReq r);
    // init ifc
    method Action initData(Addr a, Data d);
    method Action initCLine(Addr a, CacheLine d);
    method Action initDone;
    // debug ifc
    method Data getData(Addr a);
    method CacheLine getCLine(Addr a);
endinterface

interface CCMProcResp;
    method Action respLd(CCMReqId id, Data d);
    method Action respLrScAmo(Data d);
    method ActionValue#(Tuple2#(CLineByteEn, CacheLine)) respSt(CCMReqId id);
endinterface

module mkCCM#(CCMProcResp respIfc)(CCM) provisos(
    NumAlias#(mshrSz, CCMMaxReqNum),
    NumAlias#(maxDelay, CCMMaxDelay),
    Alias#(mshrIdxT, Bit#(TLog#(mshrSz))),
    Alias#(delayT, Bit#(TLog#(maxDelay))),
    Alias#(reqIdT, CCMReqId),
    NumAlias#(memAddrWidth, TSub#(LogMemNumBytes, LogCLineNumBytes)),
    Alias#(memAddrT, Bit#(memAddrWidth))
);
    // mshr
    Vector#(mshrSz, Ehr#(2, Bool)) valid <- replicateM(mkEhr(False));
    Vector#(mshrSz, Reg#(MemReq)) req <- replicateM(mkRegU);
    Vector#(mshrSz, Reg#(reqIdT)) rid <- replicateM(mkRegU);
    Vector#(mshrSz, SatDownCounter#(delayT)) delay <- replicateM(mkSatDownCounter(0));
    // memory
    RegFile#(memAddrT, CacheLine) mem <- mkRegFileFull;
    // random delay --> when can we resp
    Randomize#(delayT) randDelay <- mkGenericRandomizer;
    // wire for req
    RWire#(Tuple4#(mshrIdxT, MemReq, reqIdT, delayT)) procReqEn <- mkRWire;
    // init bit
    Reg#(Bool) inited <- mkReg(False);

    function memAddrT getMemAddr(Addr a);
        return truncate(a >> valueOf(LogCLineNumBytes));
    endfunction

    rule doResp(inited);
        // find ready to resp entry
        function Bool canResp(Integer i);
            return valid[i][0] && delay[i] == 0;
        endfunction
        Vector#(mshrSz, Integer) idxVec = genVector;
        let respIdx = findIndex(canResp, idxVec);
        when(isValid(respIdx), noAction);
        mshrIdxT idx = pack(validValue(respIdx));
        // remove entry
        valid[idx][0] <= False;

        // handle req & send resp
        MemReq r = req[idx];
        reqIdT id = rid[idx];
        let mAddr = getMemAddr(r.addr);
        CLineDataSel sel = getCLineDataSel(r.addr);
        Vector#(CLineNumData, Data) line = unpack(mem.sub(mAddr));
        if(r.op == Ld) begin
            respIfc.respLd(id, line[sel]);
        end
        else if(r.op == Lr) begin
            respIfc.respLrScAmo(line[sel]);
        end
        else if(r.op == Sc) begin
            // resp proc (Sc always succeed)
            respIfc.respLrScAmo(0);
            // get new line
            Vector#(NumBytes, Bit#(8)) curData = unpack(line[sel]);
            Vector#(NumBytes, Bit#(8)) wrData = unpack(r.data);
            function Bit#(8) getNewByte(Integer i);
                return r.byteEn[i] ? wrData[i] : curData[i];
            endfunction
            Vector#(NumBytes, Integer) byteVec = genVector;
            line[sel] = pack(map(getNewByte, byteVec));
            mem.upd(mAddr, pack(line));
        end
        else if(r.op == St) begin
            // resp proc
            let {be, wrLine} <- respIfc.respSt(id);
            // update mem
            Vector#(CLineNumBytes, Bit#(8)) curByteVec = unpack(pack(line));
            Vector#(CLineNumBytes, Bit#(8)) wrByteVec = unpack(wrLine);
            function Bit#(8) getNewByte(Integer i);
                return be[i] ? wrByteVec[i] : curByteVec[i];
            endfunction
            Vector#(CLineNumBytes, Integer) lineVec = genVector;
            mem.upd(mAddr, pack(map(getNewByte, lineVec)));
        end
        else begin
            doAssert(False, "unsupport op");
        end
    endrule

    (* fire_when_enabled, no_implicit_conditions *)
    rule canon(inited);
        // insert new req
        if(procReqEn.wget matches tagged Valid {.idx, .r, .id, .lat}) begin
            doAssert(!valid[idx][1], "cannot insert to valid entry");
            valid[idx][1] <= True;
            req[idx] <= r;
            rid[idx] <= id;
            delay[idx] <= lat;
        end
        // dec delay
        function Action decDelay(Integer i) = delay[i].dec;
        Vector#(mshrSz, Integer) idxVec = genVector;
        joinActions(map(decDelay, idxVec));
    endrule

    method Action procReq(reqIdT id, MemReq r) if(inited);
        // find empty entry
        let emptyIdx = findIndex( \== (False) , readVEhr(0, valid) );
        when(isValid(emptyIdx), noAction);
        // get random delay
        let lat <- randDelay.next;
        // set wire
        procReqEn.wset(tuple4(pack(validValue(emptyIdx)), r, id, lat));
    endmethod

    method Action initData(Addr a, Data d) if(!inited);
        let mAddr = getMemAddr(a);
        let dataSel = getCLineDataSel(a);
        Vector#(CLineNumData, Data) line = unpack(mem.sub(mAddr));
        line[dataSel] = d;
        mem.upd(mAddr, pack(line));
    endmethod

    method Action initCLine(Addr a, CacheLine d) if(!inited);
        let mAddr = getMemAddr(a);
        mem.upd(mAddr, d);
    endmethod

    method Action initDone if(!inited);
        inited <= True;
        // init randomizer
        randDelay.cntrl.init;
    endmethod

    method Data getData(Addr a);
        let mAddr = getMemAddr(a);
        let dataSel = getCLineDataSel(a);
        Vector#(CLineNumData, Data) line = unpack(mem.sub(mAddr));
        return line[dataSel];
    endmethod

    method CacheLine getCLine(Addr a);
        let mAddr = getMemAddr(a);
        return mem.sub(mAddr);
    endmethod
endmodule
