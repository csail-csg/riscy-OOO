import CacheUtils::*;
import Vector::*;
import Types::*;
import MemoryTypes::*;
import TestTypes::*;
import RegFile::*;
import Amo::*;
import MsgFifo::*;
import Assert::*;

interface RefMem;
    method Action initData(Addr a, Data d);
    method Action initCLine(Addr a, CacheLine d);
    method Action initDone;
    method Data getData(Addr a);
    method CacheLine getCLine(Addr a);
    method ActionValue#(Data) procReq(MemReq r);
endinterface

(* synthesize *)
module mkRefMem(RefMem) provisos (
    NumAlias#(memAddrWidth, TSub#(LogMemNumBytes, LogCLineNumBytes)),
    Alias#(memAddrT, Bit#(memAddrWidth)),
    Add#(memAddrWidth, a__, AddrSz)
);
    function memAddrT getMemAddr(Addr a);
        return truncate(a >> valueOf(LogCLineNumBytes));
    endfunction

    Reg#(Bool) inited <- mkReg(False);
    RegFile#(memAddrT, CacheLine) mem <- mkRegFileFull;

    method ActionValue#(Data) procReq(MemReq r) if(inited);
        let mAddr = getMemAddr(r.addr);
        CLineDataSel sel = getCLineDataSel(r.addr);
        Vector#(CLineNumData, Data) line = unpack(mem.sub(mAddr));
        Data data = line[sel];

        Data resp = 0;

        if(r.op == Ld || r.op == Lr) begin
            resp = data;
        end
        else if(r.op == St || r.op == Sc) begin
            Vector#(CLineNumData, Data) newLine = line; // new data to write
            Vector#(NumBytes, Bit#(8)) newData = unpack(data);
            Vector#(NumBytes, Bit#(8)) wrData = unpack(r.data);
            for(Integer i = 0; i < valueOf(NumBytes); i = i+1) begin
                if(r.byteEn[i]) begin
                    newData[i] = wrData[i];
                end
            end
            newLine[sel] = pack(newData);
            // write to mem
            mem.upd(mAddr, pack(newLine));
            // Sc always succeed, store doesn't need resp, so set to 0
            resp = 0;
        end
        else begin
            dynamicAssert(False, "unsupport mem op");
        end

        return resp;
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
