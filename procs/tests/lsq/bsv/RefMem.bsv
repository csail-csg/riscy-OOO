import Vector::*;
import Types::*;
import MemoryTypes::*;
import TestTypes::*;
import CCTypes::*;
import RegFile::*;
import Amo::*;
import Assert::*;

interface RefMem;
    method Action initData(Addr a, Data d);
    method Action initLine(Addr a, Line d);
    method Action initDone;
    method Data getData(Addr a);
    method Line getLine(Addr a);
    method ActionValue#(Data) procReq(MemReq r);
endinterface

(* synthesize *)
module mkRefMem(RefMem) provisos (
    NumAlias#(memAddrWidth, TLog#(TestLineNum)),
    Alias#(memAddrT, Bit#(memAddrWidth)),
    Add#(memAddrWidth, a__, AddrSz)
);
    function memAddrT getMemAddr(Addr a);
        return truncate(a >> valueOf(LgLineSzBytes));
    endfunction

    Reg#(Bool) inited <- mkReg(False);
    RegFile#(memAddrT, Line) mem <- mkRegFileFull;

    method ActionValue#(Data) procReq(MemReq r) if(inited);
        let mAddr = getMemAddr(r.addr);
        LineDataOffset sel = getLineDataOffset(r.addr);
        Line line = mem.sub(mAddr);
        Data data = line[sel];

        Data resp = 0;
        if(r.op == Ld || r.op == Lr) begin
            resp = data;
        end
        else if(r.op == St || r.op == Sc) begin
            Line newLine = line; // new data to write
            Vector#(NumBytes, Bit#(8)) newData = unpack(data);
            Vector#(NumBytes, Bit#(8)) wrData = unpack(r.data);
            for(Integer i = 0; i < valueOf(NumBytes); i = i+1) begin
                if(r.byteEn[i]) begin
                    newData[i] = wrData[i];
                end
            end
            newLine[sel] = pack(newData);
            // write to mem
            mem.upd(mAddr, newLine);
            // Sc always succeed, store doesn't need resp, so set to 0
            resp = 0;
        end
        else begin
            doAssert(False, "unsupport mem op");
        end

        return resp;
    endmethod

    method Action initData(Addr a, Data d) if(!inited);
        let mAddr = getMemAddr(a);
        let dataSel = getLineDataOffset(a);
        let line = mem.sub(mAddr);
        line[dataSel] = d;
        mem.upd(mAddr, line);
    endmethod

    method Action initLine(Addr a, Line d) if(!inited);
        let mAddr = getMemAddr(a);
        mem.upd(mAddr, d);
    endmethod

    method Action initDone if(!inited);
        inited <= True;
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
