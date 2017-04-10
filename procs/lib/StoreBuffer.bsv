`include "ProcConfig.bsv"
import Types::*;
import ProcTypes::*;
import Vector::*;
import FIFOF::*;
import FShow::*;
import Assert::*;
import Ehr::*;
import CacheUtils::*;

// store buffer data block byte size == cache line
typedef CLineNumBytes SBBlockNumBytes;
typedef TMul#(8, SBBlockNumBytes) SBBlockSz;
typedef Bit#(SBBlockSz) SBBlock;
typedef Vector#(SBBlockNumBytes, Bool) SBByteEn;

// aligned addr
typedef TSub#(AddrSz, TLog#(SBBlockNumBytes)) SBBlockAddrSz;
typedef Bit#(SBBlockAddrSz) SBBlockAddr;
function SBBlockAddr getSBBlockAddr(Addr a);
    return truncateLSB(a);
endfunction

// SB block vs. normal data
typedef TDiv#(SBBlockSz, DataSz) SBBlockNumData;
typedef Bit#(TLog#(SBBlockNumData)) SBBlockDataSel;
function SBBlockDataSel getSBBlockDataSel(Addr a);
    return truncate(a >> valueOf(TLog#(NumBytes)));
endfunction

// store buffer entry
typedef struct {
    SBBlockAddr addr;
    SBByteEn byteEn;
    SBBlock data;
} SBEntry deriving(Bits, Eq, FShow);

// result of searching (e.g. load byass)
typedef struct {
    Maybe#(SBIndex) matchIdx;
    Maybe#(Data) forwardData; // XXX data is not shifted to match load addr offset
} SBSearchRes deriving(Bits, Eq, FShow);

interface StoreBuffer;
    method Bool isEmpty;
    method Maybe#(SBIndex) getEnqIndex(Addr paddr);
    method Action enq(SBIndex idx, Addr paddr, ByteEn be, Data data);
    method ActionValue#(SBEntry) deq(SBIndex idx);
    method ActionValue#(Tuple2#(SBIndex, SBEntry)) issue;
    method SBSearchRes search(Addr paddr, ByteEn be); // load bypass/stall or atomic inst stall
    method Bool noMatch(Addr paddr, ByteEn be); // for AMO/Lr/Sc issue
    // XXX assume BE has been shifted approriately for paddr offset
    // (for load we need to do that before calling the methods)
endinterface

/*
(* synthesize *)
module mkStoreBuffer(StoreBuffer);
    // entries with valid bit
    Vector#(SBSize, Reg#(SBEntry)) entry <- replicateM(mkRegU);
    Vector#(SBSize, Reg#(Bool)) valid <- replicateM(mkReg(False));
    
    // FIFO of entries to be issued to memory
    FIFOF#(SBIndex) issueQ <- mkUGSizedFIFOF(valueOf(SBSize));
    // FIFO of empty entries
    FIFOF#(SBIndex) freeQ <- mkUGSizedFIFOF(valueOf(SBSize));
    // XXX I use UG FIFOs here, because some methods only call enq/deq in a branch
    // compiler may do somthing wrong with guard if the FIFOs are guarded
    // freeQ never overflow or underflow, so no explicit check is needed
    // issueQ never overflow, so notEmpty check is needed

    // freeQ needs initialization
    Reg#(Bool) inited <- mkReg(False);
    Reg#(SBIndex) initIdx <- mkReg(0);

    rule initFreeQ(!inited);
        freeQ.enq(initIdx);
        initIdx <= initIdx + 1;
        if(initIdx == fromInteger(valueOf(SBSize) - 1)) begin
            inited <= True;
        end
    endrule

    function Maybe#(Bit#(TLog#(n))) searchIndex(function Bool pred(t x), Vector#(n, t) vec);
        case(findIndex(pred, vec)) matches
            tagged Valid .idx: return Valid (pack(idx));
            default: return Invalid;
        endcase
    endfunction

    method Bool isEmpty;
        return !any(id, readVReg(valid));
    endmethod

    method Maybe#(SBIndex) getEnqIndex(Addr paddr);
        Vector#(SBSize, Integer) idxVec = genVector;
        // first find existing matching entry
        function Bool matchEntry(Integer i);
            return valid[i] && entry[i].addr == getSBBlockAddr(paddr);
        endfunction
        Maybe#(SBIndex) matchIdx = searchIndex(matchEntry, idxVec);
        if(isValid(matchIdx)) begin
            return matchIdx;
        end
        else begin
            // if no existing entry match, then find an empty entry
            return freeQ.notEmpty ? Valid (freeQ.first) : Invalid;
        end
    endmethod

    method Action enq(SBIndex idx, Addr paddr, ByteEn be, Data d) if(inited);
        // get data offset
        SBBlockDataSel sel = getSBBlockDataSel(paddr);
        // check whether the entry already exists
        if(valid[idx]) begin
            // existing entry: merge
            doAssert(getSBBlockAddr(paddr) == entry[idx].addr, "SB enq to existing entry addr should match");
            // update data
            Vector#(SBBlockNumData, Data) block = unpack(entry[idx].data);
            Vector#(NumBytes, Bit#(8)) origData = unpack(block[sel]);
            Vector#(NumBytes, Bit#(8)) wrData = unpack(d);
            function Bit#(8) getNewByte(Integer i) = be[i] ? wrData[i] : origData[i];
            Vector#(NumBytes, Bit#(8)) newData = map(getNewByte, genVector);
            block[sel] = pack(newData);
            // update byte enable
            Vector#(SBBlockNumData, ByteEn) byteEn = unpack(pack(entry[idx].byteEn));
            byteEn[sel] = unpack(pack(byteEn[sel]) | pack(be));
            // update entry
            entry[idx] <= SBEntry {
                addr: getSBBlockAddr(paddr),
                byteEn: unpack(pack(byteEn)),
                data: pack(block)
            };
            // this entry must have been sent to issueQ
        end
        else begin
            // new entry: set valid
            valid[idx] <= True;
            // setup entry
            Vector#(SBBlockNumData, Data) block = ?;
            block[sel] = d;
            Vector#(SBBlockNumData, ByteEn) byteEn = replicate(replicate(False));
            byteEn[sel] = be;
            entry[idx] <= SBEntry {
                addr: getSBBlockAddr(paddr),
                byteEn: unpack(pack(byteEn)),
                data: pack(block)
            };
            // send this entry to issueQ
            doAssert(issueQ.notFull, "SB issueQ should not be full");
            issueQ.enq(idx);
            // remove from freeQ
            doAssert(freeQ.notEmpty, "SB freeQ should not be empty");
            freeQ.deq;
        end
    endmethod

    method ActionValue#(SBEntry) deq(SBIndex idx) if(inited);
        doAssert(valid[idx], "SB deq entry must be valid");
        valid[idx] <= False; // set entry to invalid
        freeQ.enq(idx); // recycle entry
        return entry[idx];
    endmethod

    method ActionValue#(Tuple2#(SBIndex, SBEntry)) issue if(issueQ.notEmpty);
        issueQ.deq;
        SBIndex idx = issueQ.first;
        return tuple2(idx, entry[idx]);
    endmethod

    method SBSearchRes search(Addr paddr, ByteEn be);
        // input BE has been shifted, just pack it
        Bit#(NumBytes) ldBE = pack(be);

        // data offset within block
        SBBlockDataSel sel = getSBBlockDataSel(paddr);

        // helper to extract byteEn from entry
        function Bit#(NumBytes) getEntryBE(SBIndex idx);
            Vector#(SBBlockNumData, ByteEn) byteEn = unpack(pack(entry[idx].byteEn));
            return pack(byteEn[sel]);
        endfunction
        
        // func to determine whether the load matches a store entry
        function Bool matchEntry(Integer i);
            // entry must be valid, addr should match, byte enable should overlap
            Bool sameAddr = getSBBlockAddr(paddr) == entry[i].addr;
            Bool beOverlap = (ldBE & getEntryBE(fromInteger(i))) != 0;
            return valid[i] && sameAddr && beOverlap;
        endfunction

        // find match entry and determine return value
        Vector#(SBSize, Integer) idxVec = genVector;
        if(searchIndex(matchEntry, idxVec) matches tagged Valid .idx) begin
            // check whether bytes reading are all covered by the entry
            if((getEntryBE(idx) & ldBE) == ldBE) begin
                // fully covered, forward data
                Vector#(SBBlockNumData, Data) block = unpack(entry[idx].data);
                return SBSearchRes {
                    matchIdx: Valid (idx),
                    forwardData: Valid (block[sel])
                };
            end
            else begin
                // interact byte, not fully covered, stall the load
                return SBSearchRes {
                    matchIdx: Valid (idx),
                    forwardData: Invalid
                };
            end
        end
        else begin
            // load should go to memory
            return SBSearchRes {
                matchIdx: Invalid,
                forwardData: Invalid
            };
        end
    endmethod
endmodule
*/

/////////////////
// EHR version //
/////////////////
// method ordering: issue < enq < search < deq
// since LSQ issue will search SB after LSQ deq which enq SB, so we have enq < search
// note that deq is called by D$ resp in the background, so we make it ordered at last
// empty is used in ROB commit, so EHR port = 0
(* synthesize *)
module mkStoreBufferEhr(StoreBuffer);
    Integer emptyPort = 0;
    Integer issuePort = 0;
    Integer enqPort = 0;
    Integer searchPort = 1;
    Integer deqPort = 1;

    // entries with valid bit
    Vector#(SBSize, Ehr#(2, SBEntry)) entry <- replicateM(mkEhr(?));
    Vector#(SBSize, Ehr#(2, Bool)) valid <- replicateM(mkEhr(False));
    
    // FIFO of entries to be issued to memory
    FIFOF#(SBIndex) issueQ <- mkUGSizedFIFOF(valueOf(SBSize));
    // FIFO of empty entries
    FIFOF#(SBIndex) freeQ <- mkUGSizedFIFOF(valueOf(SBSize));
    // XXX I use UG FIFOs here, because some methods only call enq/deq in a branch
    // compiler may do somthing wrong with guard if the FIFOs are guarded
    // freeQ never overflow or underflow, so no explicit check is needed
    // issueQ never overflow, so notEmpty check is needed

    // freeQ needs initialization
    Reg#(Bool) inited <- mkReg(False);
    Reg#(SBIndex) initIdx <- mkReg(0);

    rule initFreeQ(!inited);
        freeQ.enq(initIdx);
        initIdx <= initIdx + 1;
        if(initIdx == fromInteger(valueOf(SBSize) - 1)) begin
            inited <= True;
        end
    endrule

    function Maybe#(Bit#(TLog#(n))) searchIndex(function Bool pred(t x), Vector#(n, t) vec);
        case(findIndex(pred, vec)) matches
            tagged Valid .idx: return Valid (pack(idx));
            default: return Invalid;
        endcase
    endfunction

    method Bool isEmpty;
        function Bool entryEmpty(Integer i);
            return !valid[i][emptyPort];
        endfunction
        Vector#(SBSize, Integer) idxVec = genVector;
        return all(entryEmpty, idxVec);
    endmethod

    method Maybe#(SBIndex) getEnqIndex(Addr paddr);
        Vector#(SBSize, Integer) idxVec = genVector;
        // first find existing matching entry
        function Bool matchEntry(Integer i);
            return valid[i][enqPort] && entry[i][enqPort].addr == getSBBlockAddr(paddr);
        endfunction
        Maybe#(SBIndex) matchIdx = searchIndex(matchEntry, idxVec);
        if(isValid(matchIdx)) begin
            return matchIdx;
        end
        else begin
            // if no existing entry match, then find an empty entry
            return freeQ.notEmpty ? Valid (freeQ.first) : Invalid;
        end
    endmethod

    method Action enq(SBIndex idx, Addr paddr, ByteEn be, Data d) if(inited);
        // get data offset
        SBBlockDataSel sel = getSBBlockDataSel(paddr);
        // check whether the entry already exists
        if(valid[idx][enqPort]) begin
            // existing entry: merge
            doAssert(getSBBlockAddr(paddr) == entry[idx][enqPort].addr, "SB enq to existing entry addr should match");
            // update data
            Vector#(SBBlockNumData, Data) block = unpack(entry[idx][enqPort].data);
            Vector#(NumBytes, Bit#(8)) origData = unpack(block[sel]);
            Vector#(NumBytes, Bit#(8)) wrData = unpack(d);
            function Bit#(8) getNewByte(Integer i) = be[i] ? wrData[i] : origData[i];
            Vector#(NumBytes, Bit#(8)) newData = map(getNewByte, genVector);
            block[sel] = pack(newData);
            // update byte enable
            Vector#(SBBlockNumData, ByteEn) byteEn = unpack(pack(entry[idx][enqPort].byteEn));
            byteEn[sel] = unpack(pack(byteEn[sel]) | pack(be));
            // update entry
            entry[idx][enqPort] <= SBEntry {
                addr: getSBBlockAddr(paddr),
                byteEn: unpack(pack(byteEn)),
                data: pack(block)
            };
            // this entry must have been sent to issueQ
        end
        else begin
            // new entry: set valid
            valid[idx][enqPort] <= True;
            // setup entry
            Vector#(SBBlockNumData, Data) block = ?;
            block[sel] = d;
            Vector#(SBBlockNumData, ByteEn) byteEn = replicate(replicate(False));
            byteEn[sel] = be;
            entry[idx][enqPort] <= SBEntry {
                addr: getSBBlockAddr(paddr),
                byteEn: unpack(pack(byteEn)),
                data: pack(block)
            };
            // send this entry to issueQ
            doAssert(issueQ.notFull, "SB issueQ should not be full");
            issueQ.enq(idx);
            // remove from freeQ
            doAssert(freeQ.notEmpty, "SB freeQ should not be empty");
            freeQ.deq;
        end
    endmethod

    method ActionValue#(SBEntry) deq(SBIndex idx) if(inited);
        doAssert(valid[idx][deqPort], "SB deq entry must be valid");
        valid[idx][deqPort] <= False; // set entry to invalid
        freeQ.enq(idx); // recycle entry
        return entry[idx][deqPort];
    endmethod

    method ActionValue#(Tuple2#(SBIndex, SBEntry)) issue if(issueQ.notEmpty);
        issueQ.deq;
        SBIndex idx = issueQ.first;
        return tuple2(idx, entry[idx][issuePort]);
    endmethod

    method SBSearchRes search(Addr paddr, ByteEn be);
        // input BE has been shifted, just pack it
        Bit#(NumBytes) ldBE = pack(be);

        // data offset within block
        SBBlockDataSel sel = getSBBlockDataSel(paddr);

        // helper to extract byteEn from entry
        function Bit#(NumBytes) getEntryBE(SBIndex idx);
            Vector#(SBBlockNumData, ByteEn) byteEn = unpack(pack(entry[idx][searchPort].byteEn));
            return pack(byteEn[sel]);
        endfunction
        
        // func to determine whether the load matches a store entry
        function Bool matchEntry(Integer i);
            // entry must be valid, addr should match, byte enable should overlap
            Bool sameAddr = getSBBlockAddr(paddr) == entry[i][searchPort].addr;
            Bool beOverlap = (ldBE & getEntryBE(fromInteger(i))) != 0;
            return valid[i][searchPort] && sameAddr && beOverlap;
        endfunction

        // find match entry and determine return value
        Vector#(SBSize, Integer) idxVec = genVector;
        if(searchIndex(matchEntry, idxVec) matches tagged Valid .idx) begin
            // check whether bytes reading are all covered by the entry
            if((getEntryBE(idx) & ldBE) == ldBE) begin
                // fully covered, forward data
                Vector#(SBBlockNumData, Data) block = unpack(entry[idx][searchPort].data);
                return SBSearchRes {
                    matchIdx: Valid (idx),
                    forwardData: Valid (block[sel])
                };
            end
            else begin
                // interact byte, not fully covered, stall the load
                return SBSearchRes {
                    matchIdx: Valid (idx),
                    forwardData: Invalid
                };
            end
        end
        else begin
            // load should go to memory
            return SBSearchRes {
                matchIdx: Invalid,
                forwardData: Invalid
            };
        end
    endmethod

    method Bool noMatch(Addr paddr, ByteEn be);
        // input BE has been shifted, just pack it
        Bit#(NumBytes) ldBE = pack(be);

        // data offset within block
        SBBlockDataSel sel = getSBBlockDataSel(paddr);

        // helper to extract byteEn from entry
        function Bit#(NumBytes) getEntryBE(SBIndex idx);
            Vector#(SBBlockNumData, ByteEn) byteEn = unpack(pack(entry[idx][searchPort].byteEn));
            return pack(byteEn[sel]);
        endfunction
        
        // func to determine whether the load matches a store entry
        function Bool matchEntry(Integer i);
            // entry must be valid, addr should match, byte enable should overlap
            Bool sameAddr = getSBBlockAddr(paddr) == entry[i][searchPort].addr;
            Bool beOverlap = (ldBE & getEntryBE(fromInteger(i))) != 0;
            return valid[i][searchPort] && sameAddr && beOverlap;
        endfunction

        // find match entry and determine return value
        Vector#(SBSize, Integer) idxVec = genVector;
        return !any(matchEntry, idxVec);
    endmethod
endmodule
