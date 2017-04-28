/*

Copyright (C) 2012

Arvind <arvind@csail.mit.edu>
Muralidaran Vijayaraghavan <vmurali@csail.mit.edu>

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

*/
`include "ProcConfig.bsv"
import Types::*;
import FShow::*;
import DefaultValue::*;
import MemoryTypes::*;

// OUT_OF_ORDER is never defined...
// `ifdef OUT_OF_ORDER
// `include "OoOProcTypes.bsv"
// `endif

`ifdef IN_ORDER

typedef `NUM_CORES CoreNum;
typedef Bit#(TLog#(CoreNum)) CoreId;

typedef `sizeSup SupSize;
typedef Bit#(TLog#(SupSize)) SupWaySel;
typedef Bit#(TLog#(TAdd#(SupSize, 1))) SupCnt;

typedef `NUM_EPOCHS NumEpochs;
typedef Bit#(TLog#(NumEpochs)) Epoch;

typedef `NUM_SPEC_TAGS NumSpecTags;
typedef Bit#(TLog#(NumSpecTags)) SpecTag;
typedef Bit#(NumSpecTags) SpecBits;

typedef `ROB_SIZE NumInstTags;
typedef TDiv#(NumInstTags, SupSize) SingleScalarSize;
typedef Bit#(TLog#(SingleScalarSize)) SingleScalarPtr;
typedef Bit#(TAdd#(1, TLog#(SingleScalarSize))) SingleScalarLen;

// consider ROB as a FIFO of size 2^log(NumInstTags)
// inst time is the index of the inst in the FIFO
// This indicates older/younger inst
typedef Bit#(TLog#(NumInstTags)) InstTime;

//typedef `ROB_SIZE NumRenamingTags;
//typedef Bit#(TLog#(NumRenamingTags)) RenamingTag; // index in renaming table for fast killing

`ifdef SUP_ROB
typedef struct {
    SupWaySel way; // which way in superscalar
    SingleScalarPtr ptr; // pointer within a way
    InstTime t; // inst time in ROB (for dispatch in reservation station)
} InstTag deriving(Bits, Eq, FShow);
`else
typedef Bit#(TLog#(NumInstTags)) InstTag;
`endif

typedef `SB_SIZE SBSize;
typedef Bit#(TLog#(SBSize)) SBIndex;

typedef `LDSTQ_SIZE LdStQSize;
typedef Bit#(TLog#(LdStQSize)) LdStQTag;

typedef `DDR3LLC_MAX_READS DDR3LLCMaxReads;

typedef Bit#(`LOG_DEADLOCK_CYCLES) DeadlockTimer;

typedef struct {
   Bool rv64;
   // ISA modes
   Bool h;
   Bool s;
   Bool u;
   // standard ISA extensions
   Bool m;
   Bool a;
   Bool f;
   Bool d;
   // non-standard extensions
   Bool x;
   } RiscVISASubset deriving (Bits, Eq, FShow);

instance DefaultValue#(RiscVISASubset);
  function RiscVISASubset defaultValue = RiscVISASubset{ rv64: `rv64 , h: False, s: True, u: True, m: `m , a: `a , f: `f , d: `d , x: False };
endinstance

function Data getMCPUID(RiscVISASubset isa);
  Data mcpuid = 0;
  if (isa.rv64) mcpuid = mcpuid | {2'b10, 0, 26'b00000000000000000000000000};
  // include S and I by default
  mcpuid = mcpuid | {2'b00, 0, 26'b00000001000000000100000000};
  if (isa.m) mcpuid = mcpuid | {2'b00, 0, 26'b00000000000001000000000000};
  if (isa.a) mcpuid = mcpuid | {2'b00, 0, 26'b00000000000000000000000001};
  if (isa.f) mcpuid = mcpuid | {2'b00, 0, 26'b00000000000000000000100000};
  if (isa.d) mcpuid = mcpuid | {2'b00, 0, 26'b00000000000000000000001000};
  return mcpuid;
endfunction

typedef Bit#(5) GprRIndx;
typedef Bit#(5) FpuRIndx;
typedef union tagged {
   GprRIndx Gpr;
   FpuRIndx Fpu;
} ArchRIndx deriving (Bits, Eq, FShow, Bounded);

typedef TExp#(SizeOf#(ArchRIndx)) NumArchReg;

`ifdef PHYS_REG_COUNT
typedef `PHYS_REG_COUNT NumPhyReg;
`else
typedef NumArchReg NumPhyReg;
`endif
typedef Bit#(TLog#(NumPhyReg)) PhyRIndx;

typedef struct {
    PhyRIndx indx;
    Bool isFpuReg; // need to keep track of this for fs
} PhyDst deriving (Bits, Eq, FShow);

typedef struct {
    Maybe#(ArchRIndx) src1;
    Maybe#(ArchRIndx) src2;
    Maybe#(FpuRIndx) src3;
    Maybe#(ArchRIndx) dst;
} ArchRegs deriving (Bits, Eq, FShow);

typedef struct {
    Maybe#(PhyRIndx) src1;
    Maybe#(PhyRIndx) src2;
    Maybe#(PhyRIndx) src3;
    Maybe#(PhyDst) dst;
} PhyRegs deriving (Bits, Eq, FShow);

typedef struct {
    Bool src1;
    Bool src2;
    Bool src3;
    Bool dst;
} RegsReady deriving(Bits, Eq, FShow);

function Bool allRegsReady(RegsReady x);
    return x.src1 && x.src2 && x.src3 && x.dst;
endfunction

typedef enum {
   Load    = 7'b0000011,
   LoadFp  = 7'b0000111,
   MiscMem = 7'b0001111,
   OpImm   = 7'b0010011,
   Auipc   = 7'b0010111,
   OpImm32 = 7'b0011011,
   Store   = 7'b0100011,
   StoreFp = 7'b0100111,
   Amo     = 7'b0101111,
   Op      = 7'b0110011,
   Lui     = 7'b0110111,
   Op32    = 7'b0111011,
   Fmadd   = 7'b1000011,
   Fmsub   = 7'b1000111,
   Fnmsub  = 7'b1001011,
   Fnmadd  = 7'b1001111,
   OpFp    = 7'b1010011,
   Branch  = 7'b1100011,
   Jalr    = 7'b1100111,
   Jal     = 7'b1101111,
   System  = 7'b1110011
   } Opcode deriving(Bits, Eq, FShow);

typedef enum {
   CSRfflags    = 'h001,
   CSRfrm       = 'h002,
   CSRfcsr      = 'h003,
   CSRstoreaddr = 'h008,
   CSRstore8    = 'h009,
   CSRstore16   = 'h00a,
   CSRstore32   = 'h00b,
   CSRload8     = 'h00d,
   CSRload16    = 'h00e,
   CSRload32    = 'h00f,
   CSRstats     = 'h0c0,
   CSRsstatus   = 'h100,
   CSRstvec     = 'h101,
   CSRsie       = 'h104,
   CSRstimecmp  = 'h121,
   CSRsscratch  = 'h140,
   CSRsepc      = 'h141,
   CSRsip       = 'h144,
   CSRsptbr     = 'h180,
   CSRsasid     = 'h181,
   CSRhstatus   = 'h200,
   CSRhtvec     = 'h201,
   CSRhepc      = 'h241,
   CSRmstatus   = 'h300,
   CSRmtvec     = 'h301,
   CSRmtdeleg   = 'h302,
   CSRmie       = 'h304,
   CSRmtimecmp  = 'h321,
   CSRmscratch  = 'h340,
   CSRmepc      = 'h341,
   CSRmcause    = 'h342,
   CSRmbadaddr  = 'h343,
   CSRmip       = 'h344,
   CSRmbase     = 'h380,
   CSRmbound    = 'h381,
   CSRmibase    = 'h382,
   CSRmibound   = 'h383,
   CSRmdbase    = 'h384,
   CSRmdbound   = 'h385,
   CSRsup0      = 'h500,
   CSRsup1      = 'h501,
   CSRepc       = 'h502,
   CSRbadvaddr  = 'h503,
   CSRptbr      = 'h504,
   CSRasid      = 'h505,
   CSRcount     = 'h506,
   CSRcompare   = 'h507,
   CSRevec      = 'h508,
   CSRcause     = 'h509,
   CSRstatus    = 'h50a,
   CSRhartid    = 'h50b,
   CSRimpl      = 'h50c,
   CSRfatc      = 'h50d,
   //CSRsendipi   = 'h50e, // [sizhuo] where are these two coming from?
   //CSRclearipi  = 'h50f,
   CSRtohost    = 'h51e,
   CSRfromhost  = 'h51f,
   CSRmtime     = 'h701,
   CSRmtimeh    = 'h741,
   CSRmtohost   = 'h780,
   CSRmfromhost = 'h781,
   CSRmreset    = 'h782,
   CSRsend_ipi  = 'h783,
   // user-defined non-standard CSR: rw in user space
   CSRterminate = 'h800,
   /////////////
   CSRcyclew    = 'h900,
   CSRtimew     = 'h901,
   CSRinstretw  = 'h902,
   CSRcyclehw   = 'h980,
   CSRtimehw    = 'h981,
   CSRinstrethw = 'h982,
   CSRstimew    = 'ha01,
   CSRstimehw   = 'ha81,
   CSRcycle     = 'hc00,
   CSRtime      = 'hc01,
   CSRinstret   = 'hc02,
   CSRcycleh    = 'hc80,
   CSRtimeh     = 'hc81,
   CSRinstreth  = 'hc82,
   CSRuarch0    = 'hcc0,
   CSRuarch1    = 'hcc1,
   CSRuarch2    = 'hcc2,
   CSRuarch3    = 'hcc3,
   CSRuarch4    = 'hcc4,
   CSRuarch5    = 'hcc5,
   CSRuarch6    = 'hcc6,
   CSRuarch7    = 'hcc7,
   CSRuarch8    = 'hcc8,
   CSRuarch9    = 'hcc9,
   CSRuarch10   = 'hcca,
   CSRuarch11   = 'hccb,
   CSRuarch12   = 'hccc,
   CSRuarch13   = 'hccd,
   CSRuarch14   = 'hcce,
   CSRuarch15   = 'hccf,
   CSRstime     = 'hd01,
   CSRscause    = 'hd42,
   CSRsbadaddr  = 'hd43,
   CSRstimeh    = 'hd81,
   CSRmcpuid    = 'hf00,
   CSRmimpid    = 'hf01,
   CSRmhartid   = 'hf10
   } CSR deriving(Bits, Eq, FShow);

typedef enum {Unsupported, Amo, Alu, Ld, St, Lr, Sc, J, Jr, Br, Csr, Auipc, Priv, Interrupt, Fence, SFence, Fpu, Sret, Mrts} IType deriving(Bits, Eq, FShow);
typedef enum {Eq, Neq, Lt, Ltu, Ge, Geu, AT, NT} BrFunc deriving(Bits, Eq, FShow);
typedef enum {Add, Addw, Sub, Subw, And, Or, Xor, Slt, Sltu, Sll, Sllw, Sra, Sraw, Srl, Srlw, Csrw, Csrs, Csrc} AluFunc deriving(Bits, Eq, FShow);
typedef enum {Mul, Mulh, Div, Rem} MulDivFunc deriving(Bits, Eq, FShow);
typedef enum {Signed, Unsigned, SignedUnsigned} MulDivSign deriving(Bits, Eq, FShow);
typedef struct {
    MulDivFunc  func;
    Bool        w;
    MulDivSign  sign;
} MulDivInst deriving(Bits, Eq, FShow);
typedef enum {
   FAdd, FSub, FMul, FDiv, FSqrt,
   FSgnj, FSgnjn, FSgnjx,
   FMin, FMax,
   FCvt_FF,
   FCvt_WF, FCvt_WUF, FCvt_LF, FCvt_LUF,
   FCvt_FW, FCvt_FWU, FCvt_FL, FCvt_FLU,
   FEq, FLt, FLe,
   FClass, FMv_XF, FMv_FX,
   FMAdd, FMSub, FNMSub, FNMAdd
   } FpuFunc deriving(Bits, Eq, FShow);
typedef enum {
    Single,
    Double
} FpuPrecision deriving(Bits, Eq, FShow);
typedef struct {
    FpuFunc         func;
    RVRoundMode     rm;
    FpuPrecision    precision;
} FpuInst deriving(Bits, Eq, FShow);

// LdStInst and AmoInst are defined in Types.bsv
typedef union tagged {
   AluFunc     Alu;
   BrFunc      Br;
   MemInst     Mem;
   MulDivInst  MulDiv;
   FpuInst     Fpu;
   void        Other;
   } ExecFunc deriving(Bits, Eq, FShow);

// Rounding Modes (encoding by risc-v, not general fpu)
typedef enum {
   RNE  = 3'b000,
   RTZ  = 3'b001,
   RDN  = 3'b010,
   RUP  = 3'b011,
   RMM  = 3'b100,
   RDyn = 3'b111
   } RVRoundMode deriving(Bits, Eq, FShow);

// helper functions
function Bool isAluFunc(ExecFunc f);
   return f matches tagged Alu .alu_f ? True : False;
endfunction
function Bool isMulDivFunc(ExecFunc f);
   return f matches tagged MulDiv .muldiv_f ? True : False;
endfunction
function Bool isBrFunc(ExecFunc f);
   return f matches tagged Br .br_f ? True : False;
endfunction
function Bool isFpuFunc(ExecFunc f);
  return f matches tagged Fpu .fpu_f ? True : False;
endfunction
function Bool isAmoFunc(ExecFunc f);
   return f matches tagged Mem .mem_inst ? mem_inst.mem_func == Amo : False;
endfunction

typedef enum {
  InstAddrMisaligned  = 4'd0,
  InstAccessFault     = 4'd1,
  IllegalInst         = 4'd2,
  Breakpoint          = 4'd3,
  LoadAddrMisaligned  = 4'd4,
  LoadAccessFault     = 4'd5,
  StoreAddrMisaligned = 4'd6,
  StoreAccessFault    = 4'd7,
  EnvCallU            = 4'd8,
  EnvCallS            = 4'd9,
  EnvCallH            = 4'd10,
  EnvCallM            = 4'd11,
  IllegalException    = 4'd15 // to get a 4-bit implementation
} Exception deriving(Bits, Eq, FShow);

typedef enum {
  SoftwareInterrupt   = 4'd0,
  TimerInterrupt      = 4'd1,
  HostInterrupt       = 4'd2,
  IllegalInterrupt    = 4'd15 // to get 4-bit implementation
} Interrupt deriving(Bits, Eq, FShow);

// Traps are either an exception or an interrupt
typedef union tagged {
  Exception Exception;
  Interrupt Interrupt;
} Trap deriving(Bits, Eq, FShow);

typedef struct {
  Bit#(2) prv;
  Bit#(3) frm;
  Bool f_enabled;
  Bool x_enabled;
} CsrState deriving (Bits, Eq, FShow);

typedef struct {
  Addr  pc;
  Addr  nextPc;
  IType iType;
  Bool  taken;
  Bool  mispredict;
} Redirect deriving (Bits, Eq, FShow);

typedef struct {
    Addr pc;
    Addr nextPc;
    Bool taken;
    Bool mispredict;
} ControlFlow deriving (Bits, Eq, FShow);

typedef struct {
    DecodedInst dInst;
    ArchRegs    regs;
    Bool        illegalInst;
} DecodeResult deriving(Bits, Eq, FShow);

typedef Bit#(32) ImmData; // 32-bit decoded immediate data

typedef struct {
  IType           iType;
  ExecFunc        execFunc;
  Maybe#(CSR)     csr;
  Maybe#(ImmData) imm;
} DecodedInst deriving(Bits, Eq, FShow);

function Maybe#(Data) getDInstImm(DecodedInst dInst);
    return dInst.imm matches tagged Valid .d ? Valid (signExtend(d)) : Invalid;
endfunction

typedef struct {
    Data        data;
    Data        csrData;
    Addr        addr;
    ControlFlow controlFlow;
} ExecResult deriving(Bits, Eq, FShow);

typedef struct {
  Bit#(2) prv;
  Asid    asid;
  Bit#(5) vm;
  Addr    base;
  Addr    bound;
} VMInfo deriving(Bits, Eq, FShow);

// Op
Bit#(3) fnADD   = 3'b000;
Bit#(3) fnSLL   = 3'b001;
Bit#(3) fnSLT   = 3'b010;
Bit#(3) fnSLTU  = 3'b011;
Bit#(3) fnXOR   = 3'b100;
Bit#(3) fnSR    = 3'b101;
Bit#(3) fnOR    = 3'b110;
Bit#(3) fnAND   = 3'b111;

Bit#(7) opALU1   = 7'b0000000;
Bit#(7) opALU2   = 7'b0100000;
Bit#(7) opMULDIV = 7'b0000001;

Bit#(3) fnMUL    = 3'b000;
Bit#(3) fnMULH   = 3'b001;
Bit#(3) fnMULHSU = 3'b010;
Bit#(3) fnMULHU  = 3'b011;
Bit#(3) fnDIV    = 3'b100;
Bit#(3) fnDIVU   = 3'b101;
Bit#(3) fnREM    = 3'b110;
Bit#(3) fnREMU   = 3'b111;

// Branch
Bit#(3) fnBEQ   = 3'b000;
Bit#(3) fnBNE   = 3'b001;
Bit#(3) fnBLT   = 3'b100;
Bit#(3) fnBGE   = 3'b101;
Bit#(3) fnBLTU  = 3'b110;
Bit#(3) fnBGEU  = 3'b111;

// Load
Bit#(3) fnLB    = 3'b000;
Bit#(3) fnLH    = 3'b001;
Bit#(3) fnLW    = 3'b010;
Bit#(3) fnLD    = 3'b011;
Bit#(3) fnLBU   = 3'b100;
Bit#(3) fnLHU   = 3'b101;
Bit#(3) fnLWU   = 3'b110;

// Store
Bit#(3) fnSB    = 3'b000;
Bit#(3) fnSH    = 3'b001;
Bit#(3) fnSW    = 3'b010;
Bit#(3) fnSD    = 3'b011;

// Amo
Bit#(5) fnLR      = 5'b00010;
Bit#(5) fnSC      = 5'b00011;
Bit#(5) fnAMOSWAP = 5'b00001;
Bit#(5) fnAMOADD  = 5'b00000;
Bit#(5) fnAMOXOR  = 5'b00100;
Bit#(5) fnAMOAND  = 5'b01100;
Bit#(5) fnAMOOR   = 5'b01000;
Bit#(5) fnAMOMIN  = 5'b10000;
Bit#(5) fnAMOMAX  = 5'b10100;
Bit#(5) fnAMOMINU = 5'b11000;
Bit#(5) fnAMOMAXU = 5'b11100;

// FPU
Bit#(2) fmtS      = 2'b00;
Bit#(2) fmtD      = 2'b01;
Bit#(5) opFADD    = 5'b00000;
Bit#(5) opFSUB    = 5'b00001;
Bit#(5) opFMUL    = 5'b00010;
Bit#(5) opFDIV    = 5'b00011;
Bit#(5) opFSQRT   = 5'b01011;
Bit#(5) opFSGNJ   = 5'b00100;
Bit#(5) opFMINMAX = 5'b00101;
Bit#(5) opFCMP    = 5'b10100;
Bit#(5) opFMV_XF  = 5'b11100; // FCLASS also
Bit#(5) opFMV_FX  = 5'b11110;
Bit#(5) opFCVT_FF = 5'b01000;
Bit#(5) opFCVT_WF = 5'b11000;
Bit#(5) opFCVT_FW = 5'b11010;

//MiscMem
Bit#(3) fnFENCE  = 3'b000;
Bit#(3) fnFENCEI = 3'b001;

// System
Bit#(3) fnPRIV   = 3'b000;
Bit#(3) fnCSRRW  = 3'b001;
Bit#(3) fnCSRRS  = 3'b010;
Bit#(3) fnCSRRC  = 3'b011;
Bit#(3) fnCSRRWI = 3'b101;
Bit#(3) fnCSRRSI = 3'b110;
Bit#(3) fnCSRRCI = 3'b111;

Bit#(12) privSCALL    = 12'h000;
Bit#(12) privSBREAK   = 12'h001;
Bit#(12) privSRET     = 12'h100;
Bit#(12) privSFENCEVM = 12'h101;
Bit#(12) privWFI      = 12'h102;
Bit#(12) privHRTS     = 12'h205;
Bit#(12) privMRTS     = 12'h305;
Bit#(12) privMRTH     = 12'h306;

Data _MSTATUS_IE        = 'h00000001;
Data _MSTATUS_PRV       = 'h00000006;
Data _MSTATUS_IE1       = 'h00000008;
Data _MSTATUS_PRV1      = 'h00000030;
Data _MSTATUS_IE2       = 'h00000040;
Data _MSTATUS_PRV2      = 'h00000180;
Data _MSTATUS_IE3       = 'h00000200;
Data _MSTATUS_PRV3      = 'h00000C00;
Data _MSTATUS_FS        = 'h00003000;
Data _MSTATUS_XS        = 'h0000C000;
Data _MSTATUS_MPRV      = 'h00010000;
Data _MSTATUS_VM        = 'h003E0000;
Data _MSTATUS_SD        = {1'b1, 'b0};

Data _SSTATUS_IE        = 'h00000001;
Data _SSTATUS_PIE       = 'h00000008;
Data _SSTATUS_PS        = 'h00000010;
Data _SSTATUS_FS        = 'h00003000;
Data _SSTATUS_XS        = 'h0000C000;
Data _SSTATUS_MPRV      = 'h00010000;
Data _SSTATUS_TIE       = 'h01000000;
Data _SSTATUS_SD        = {1'b1, 'b0};

Data _MIP_SSIP          = 'h00000002;
Data _MIP_HSIP          = 'h00000004;
Data _MIP_MSIP          = 'h00000008;
Data _MIP_STIP          = 'h00000020;
Data _MIP_HTIP          = 'h00000040;
Data _MIP_MTIP          = 'h00000080;

Data _SIP_SSIP          = _MIP_SSIP;
Data _SIP_STIP          = _MIP_STIP;

Bit#(2) prvU = 0;
Bit#(2) prvS = 1;
Bit#(2) prvH = 2;
Bit#(2) prvM = 3;

Bit#(5) vmMbare = 0;
Bit#(5) vmMbb   = 1;
Bit#(5) vmMbbid = 2;
Bit#(5) vmSv32  = 8;
Bit#(5) vmSv39  = 9;
Bit#(5) vmSv48  = 10;
Bit#(5) vmSv57  = 11;
Bit#(5) vmSv64  = 12;

function Bool isSystem(IType iType) = (iType == Priv || iType == Csr || iType == Unsupported || iType == Interrupt || iType == SFence || iType == Fence || iType == Sret || iType == Mrts);

// instruction requires replaying (i.e. fetch next instruction after current
// instruction commits)
function Bool doReplay(IType iType) = isSystem(iType);

function Bool isFpuInst(IType iType) = (iType == Fpu);

function Bool isMemInst(IType iType) = (iType == Ld || iType == St || iType == Lr || iType == Sc || iType == Amo);

function Fmt showInst(Instruction inst);
  Fmt ret = fshow("");

  Opcode opcode = unpack(inst[  6 :  0 ]);
  let rd     = inst[ 11 :  7 ];
  let funct3 = inst[ 14 : 12 ];
  let rs1    = inst[ 19 : 15 ];
  let rs2    = inst[ 24 : 20 ];
  let funct7 = inst[ 31 : 25 ];

  Bit#(32) immI   = signExtend(inst[31:20]);
  Bit#(32) immS   = signExtend({ inst[31:25], inst[11:7] });
  Bit#(32) immB   = signExtend({ inst[31], inst[7], inst[30:25], inst[11:8], 1'b0});
  Bit#(32) immU   = { inst[31:12], 12'b0 };
  Bit#(32) immJ   = signExtend({ inst[31], inst[19:12], inst[20], inst[30:25], inst[24:21], 1'b0});

  case (opcode)
    OpImm:
    begin
      ret = case (funct3)
        fnADD: fshow("addi");
        fnSLT: fshow("slti");
        fnSLTU: fshow("sltiu");
        fnAND: fshow("andi");
        fnOR: fshow("ori");
        fnXOR: fshow("xori");
        fnSLL: fshow("slli");
        fnSR: (immI[10] == 0 ? fshow("srli") : fshow("srai"));
      endcase;
      ret = ret + fshow(" ") + fshow(rd) + fshow(" = ") + fshow(rs1) + fshow(" ");
      ret = ret + (case (funct3)
        fnSLL, fnSR: fshow(immI[5:0]);
        default: fshow(immI);
      endcase);
    end

    OpImm32:
    begin
      ret = case (funct3)
        fnADD: fshow("addiw");
        fnSLL: fshow("slliw");
        fnSR: (immI[10] == 0 ? fshow("srliw") : fshow("sraiw"));
      endcase;
      ret = ret + fshow(" ") + fshow(rd) + fshow(" = ") + fshow(rs1) + fshow(" ");
      ret = ret + (case (funct3)
        fnSLL, fnSR: fshow(immI[4:0]);
        default: fshow(immI);
      endcase);
    end

    Op:
    begin
      ret = case (funct3)
        fnADD: (immI[10] == 0 ? fshow("add") : fshow("sub"));
        fnSLT: fshow("slt");
        fnSLTU: fshow("sltu");
        fnAND: fshow("and");
        fnOR: fshow("or");
        fnXOR: fshow("xor");
        fnSLL: fshow("sll");
        fnSR: (immI[10] == 0 ? fshow("srl") : fshow("sra"));
      endcase;
      ret = ret + fshow(" ") + fshow(rd) + fshow(" = ") + fshow(rs1) + fshow(" ") + fshow(rs2);
    end

    Op32:
    begin
      ret = case (funct3)
        fnADD: (immI[10] == 0 ? fshow("addw") : fshow("subw"));
        fnSLL: fshow("sllw");
        fnSR: (immI[10] == 0 ? fshow("srlw") : fshow("sraw"));
      endcase;
      ret = ret + fshow(" ") + fshow(rd) + fshow(" = ") + fshow(rs1) + fshow(" ") + fshow(rs2);
    end

    Lui:
      ret = fshow("lui ") + fshow(rd) + fshow(" ") + fshow(immU);

    Auipc:
      ret = fshow("auipc ") + fshow(rd) + fshow(" ") + fshow(immU);

    Jal:
      ret = fshow("jal ") + fshow(rd) + fshow(" ") + fshow(immJ);

    Jalr:
      ret = fshow("jalr ") + fshow(rd) + fshow(" ") + fshow(rs1) + fshow(" ") + fshow(immI);

    Branch:
    begin
      ret = case(funct3)
        fnBEQ: fshow("beq");
        fnBNE: fshow("bne");
        fnBLT: fshow("blt");
        fnBLTU: fshow("bltu");
        fnBGE: fshow("bge");
        fnBGEU: fshow("bgeu");
      endcase;
      ret = ret + fshow(" ") + fshow(rs1) + fshow(" ") + fshow(rs2) + fshow(" ") + fshow(immB);
    end

    Load:
    begin
      ret = case(funct3)
        fnLB: fshow("lb");
        fnLH: fshow("lh");
        fnLW: fshow("lw");
        fnLD: fshow("ld");
        fnLBU: fshow("lbu");
        fnLHU: fshow("lhu");
        fnLWU: fshow("lwu");
      endcase;
      ret = ret + fshow(" ") + fshow(rd) + fshow(" = ") + fshow(rs1) + fshow(" ") + fshow(immI);
    end

    Store:
    begin
      ret = case(funct3)
        fnSB: fshow("sb");
        fnSH: fshow("sh");
        fnSW: fshow("sw");
        fnSD: fshow("sd");
      endcase;
      ret = ret + fshow(" ") + fshow(rs1) + fshow(" ") + fshow(rs2) + fshow(" ") + fshow(immS);
    end

    MiscMem:
    begin
      ret = case (funct3)
        fnFENCE: fshow("fence");
        fnFENCEI: fshow("fence.i");
      endcase;
    end

    System:
    begin
      case (funct3)
        fnCSRRW, fnCSRRS, fnCSRRC, fnCSRRWI, fnCSRRSI, fnCSRRCI:
        begin
          ret = case(funct3)
            fnCSRRW: fshow("csrrw");
            fnCSRRC: fshow("csrrc");
            fnCSRRS: fshow("csrrs");
            fnCSRRWI: fshow("csrrwi");
            fnCSRRCI: fshow("csrrci");
            fnCSRRSI: fshow("csrrsi");
          endcase;
          ret = ret + fshow(" ") + fshow(rd) + fshow(" ") + fshow(immI) + fshow(" ") + fshow(rs1);
        end

        fnPRIV:
        begin
          ret = case (truncate(immI))
            privSCALL: fshow("scall");
            privSBREAK: fshow("sbreak");
            privSRET: fshow("sret");
            privSFENCEVM: (fshow("sfence.vm ") + fshow(rs1));
            privWFI: fshow("wfi");
            privHRTS: fshow("hrts");
            privMRTS: fshow("mrts");
            privMRTH: fshow("mrth");
          endcase;
        end

        default:
          ret = fshow("SYSTEM not implemented");
      endcase
    end
    /*
    opLB:
      ret = fshow("lb ") + fshow(rt) + fshow(" = ") + fshow(rs) + fshow(" ") + fshow(imm);

    opLH:
      ret = fshow("lh ") + fshow(rt) + fshow(" = ") + fshow(rs) + fshow(" ") + fshow(imm);

    opLW:
      ret = fshow("lw ") + fshow(rt) + fshow(" = ") + fshow(rs) + fshow(" ") + fshow(imm);

    opLBU:
      ret = fshow("lbu ") + fshow(rt) + fshow(" = ") + fshow(rs) + fshow(" ") + fshow(imm);

    opLHU:
      ret = fshow("lhu ") + fshow(rt) + fshow(" = ") + fshow(rs) + fshow(" ") + fshow(imm);

    opSB:
      ret = fshow("sb ") + fshow(rs) + fshow(" ") + fshow(rt) + fshow(" ") + fshow(imm);

    opSH:
      ret = fshow("sh ") + fshow(rs) + fshow(" ") + fshow(rt) + fshow(" ") + fshow(imm);

    opSW:
      ret = fshow("sw ") + fshow(rs) + fshow(" ") + fshow(rt) + fshow(" ") + fshow(imm);
*/
    default:
      ret = fshow("nop");
  endcase

  return ret;

endfunction

`endif
