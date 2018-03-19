import Vector::*;
import Fifo::*;
import Types::*;
import MemoryTypes::*;
import CCTypes::*;
import TestTypes::*;
import RegFile::*;
import Amo::*;
import MsgFifo::*;
import Assert::*;
import Ehr::*;
import Randomizable::*;

interface CCM;
    method Action procReq(CCMReqId id, MemReq r);
    // init ifc
    method Action initData(Addr a, Data d);
    method Action initLine(Addr a, Line d);
    method Action initDone;
    // debug ifc
    method Data getData(Addr a);
    method Line getLine(Addr a);
endinterface

interface CCMProcResp;
    method Action respLd(CCMReqId id, Data d);
    method Action respLrScAmo(Data d);
    method ActionValue#(Tuple2#(LineByteEn, Line)) respSt(CCMReqId id);
endinterface

module mkCCM#(CCMProcResp respIfc)(CCM) provisos(
    NumAlias#(mshrSz, CCMMaxReqNum),
    NumAlias#(maxDelay, CCMMaxDelay),
    Alias#(mshrIdxT, Bit#(TLog#(mshrSz))),
    Alias#(delayT, Bit#(TLog#(maxDelay))),
    Alias#(reqIdT, CCMReqId),
    NumAlias#(memAddrWidth, TLog#(TestLineNum)),
    Alias#(memAddrT, Bit#(memAddrWidth))
);
    // rule ordering: resp < dec delay < req

    // input Q
    Fifo#(1, Tuple2#(CCMReqId, MemReq)) procReqQ <- mkBypassFifo;
    // mshr
    Vector#(mshrSz, Ehr#(2, Bool)) valid <- replicateM(mkEhr(False));
    let valid_resp = getVEhrPort(valid, 0); // write
    let valid_req  = getVEhrPort(valid, 1); // write
    Vector#(mshrSz, Reg#(MemReq)) req <- replicateM(mkRegU);
    Vector#(mshrSz, Reg#(reqIdT)) rid <- replicateM(mkRegU);
    Vector#(mshrSz, Ehr#(2, delayT)) delay <- replicateM(mkEhr(0));
    let delay_resp = getVEhrPort(delay, 0);
    let delay_dec = getVEhrPort(delay, 0); // write
    let delay_req = getVEhrPort(delay, 1); // write
    // memory
    RegFile#(memAddrT, Line) mem <- mkRegFileFull;
    // random delay --> when can we resp
    Randomize#(delayT) randDelay <- mkGenericRandomizer;
    // init bit
    Reg#(Bool) inited <- mkReg(False);

    function memAddrT getMemAddr(Addr a);
        return truncate(a >> valueOf(LgLineSzBytes));
    endfunction

    rule doResp(inited);
        // find ready to resp entry
        function Bool canResp(mshrIdxT i);
            return valid_resp[i] && delay_resp[i] == 0;
        endfunction
        Vector#(mshrSz, mshrIdxT) idxVec = genWith(fromInteger);
        Maybe#(mshrIdxT) respIdx = find(canResp, idxVec);
        when(isValid(respIdx), noAction);
        mshrIdxT idx = validValue(respIdx);
        // remove entry
        valid_resp[idx] <= False;

        // handle req & send resp
        MemReq r = req[idx];
        reqIdT id = rid[idx];
        let mAddr = getMemAddr(r.addr);
        LineDataOffset sel = getLineDataOffset(r.addr);
        Line line = mem.sub(mAddr);
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
            mem.upd(mAddr, line);
        end
        else if(r.op == St) begin
            // resp proc
            let {be, wrLine} <- respIfc.respSt(id);
            // update mem
            Vector#(LineSzBytes, Bit#(8)) curByteVec = unpack(pack(line));
            Vector#(LineSzBytes, Bit#(8)) wrByteVec = unpack(pack(wrLine));
            function Bit#(8) getNewByte(Integer i);
                return be[i] ? wrByteVec[i] : curByteVec[i];
            endfunction
            Vector#(LineSzBytes, Integer) lineVec = genVector;
            mem.upd(mAddr, unpack(pack(map(getNewByte, lineVec))));
        end
        else begin
            doAssert(False, "unsupport op");
        end
    endrule

    (* fire_when_enabled, no_implicit_conditions *)
    rule decDelay(inited);
        function Action decDelay(Integer i);
        action
            if(delay_dec[i] > 0) begin
                delay_dec[i] <= delay_dec[i] - 1;
            end
        endaction
        endfunction
        Vector#(mshrSz, Integer) idxVec = genVector;
        joinActions(map(decDelay, idxVec));
    endrule

    rule doReq(inited);
        procReqQ.deq;
        let {id, r} = procReqQ.first;
        // find an empty entry
        function Bool isEmpty(mshrIdxT i) = !valid_req[i];
        Vector#(mshrSz, mshrIdxT) idxVec = genWith(fromInteger);
        Maybe#(mshrIdxT) emptyIdx = find(isEmpty, idxVec);
        when(isValid(emptyIdx), noAction);
        mshrIdxT idx = validValue(emptyIdx);
        // get random delay
        let lat <- randDelay.next;
        // setup entry
        valid_req[idx] <= True;
        req[idx] <= r;
        rid[idx] <= id;
        delay_req[idx] <= lat;
    endrule

    method Action procReq(reqIdT id, MemReq r) if(inited);
        procReqQ.enq(tuple2(id, r));
    endmethod

    method Action initData(Addr a, Data d) if(!inited);
        let mAddr = getMemAddr(a);
        let dataSel = getLineDataOffset(a);
        Line line = mem.sub(mAddr);
        line[dataSel] = d;
        mem.upd(mAddr, line);
    endmethod

    method Action initLine(Addr a, Line d) if(!inited);
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
        let dataSel = getLineDataOffset(a);
        let line = mem.sub(mAddr);
        return line[dataSel];
    endmethod

    method Line getLine(Addr a);
        let mAddr = getMemAddr(a);
        return mem.sub(mAddr);
    endmethod
endmodule
