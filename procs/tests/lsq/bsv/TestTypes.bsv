`include "ProcConfig.bsv"
import Ehr::*;
import Types::*;
import MemoryTypes::*;
import ProcTypes::*;
import CCTypes::*;

// mem req
typedef struct {
    MemOp op;
    Addr addr;
    ByteEn byteEn;
    Data data;
} MemReq deriving(Bits, Eq, FShow);

// test info
typedef `ROB_SIZE TestNum;
typedef Bit#(TLog#(TAdd#(TestNum, 1))) TestCnt;
typedef Bit#(TLog#(TestNum)) TestId; // this is also used in InstTag

// convert test id to/from inst tag
function InstTag toInstTag(TestId id);
    return InstTag {way: 0, ptr: 0, t: id};
endfunction

function TestId fromInstTag(InstTag tag) = tag.t;

// timeout
typedef 10000 MaxTimeOut;
typedef Bit#(TLog#(TAdd#(MaxTimeOut, 1))) TimeOut;

// CCM & TLB params
typedef 16 CCMMaxReqNum;
typedef 128 CCMMaxDelay;

typedef Bit#(TMax#(SizeOf#(LdQTag), SizeOf#(SBIndex))) CCMReqId;

typedef 16 TLBMaxReqNum;
typedef 16 TLBMaxDelay;

typedef struct {
    TestId testId;
    LdStQTag lsqTag;
    SpecTag specTag;
    ByteEn shiftedBE;
} DelayTLBReq deriving(Bits, Eq, FShow);

// test req generator
typedef 4 TestLineNum; // number of cache lines
typedef 2 TestDataSelNum; // which data in a line

typedef TAdd#(TLog#(TestLineNum), LgLineSzBytes) LogMemSzBytes;

// offset within data
typedef Bit#(TLog#(NumBytes)) ByteOffset;

// byte, half word, word, double word
typedef enum {
    B, H, W, D
} AccessRange deriving(Bits, Eq, FShow, Bounded);

// mem func for testing
typedef enum {
    Ld, St, Lr, Sc
} TestMemFunc deriving(Bits, Eq, FShow, Bounded);

function MemFunc toMemFunc(TestMemFunc f);
    case(f)
        Ld: return Ld;
        St: return St;
        Lr: return Lr;
        Sc: return Sc;
        default: return ?;
    endcase
endfunction

function MemOp toMemOp(TestMemFunc f);
    case(f)
        Ld: return Ld;
        St: return St;
        Lr: return Lr;
        Sc: return Sc;
        default: return ?;
    endcase
endfunction

// generate test req
function Tuple2#(Addr, MemInst) getAddrMemInst(
    LineAddr lineAddr, LineDataOffset dataSel,
    ByteOffset off, AccessRange r, TestMemFunc func
);
    Bit#(NumBytes) be = 0;
    case(r)
        B: begin
            be[0] = maxBound;
        end
        H: begin
            off[0] = 0;
            be[1:0] = maxBound;
        end
        W: begin
            off[1:0] = 0;
            be[3:0] = maxBound;
        end
        D: begin
            off = 0;
            be = maxBound;
        end
    endcase
    Addr a = {lineAddr, dataSel, off};
    let mInst = MemInst {
        mem_func: toMemFunc(func),
        amo_func: ?, // AMO is not tested now
        unsignedLd: True, // always do unsign now
        byteEn: unpack(be),
        // no fence bit for now
        aq: False,
        rl: False
    };
    return tuple2(a, mInst);
endfunction

// test FSM
typedef enum {
    StartInit,
    WaitInitDone,
    Testing,
    StartCheck,
    WaitCheckDone
} TestState deriving(Bits, Eq, FShow);

// test req & ref resp
typedef struct {
    MemInst memInst;
    Addr paddr;
    Data stData; // unshifted store data
    Data resp; // Ld/Lr/Sc resp
} TestEntry deriving(Bits, Eq, FShow);
