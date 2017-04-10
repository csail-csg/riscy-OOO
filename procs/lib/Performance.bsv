import FShow::*;

// performance counter typedefs

// query performance counters in each stage/module
typedef enum {
  LdCnt,
  LdMissCnt,
  LdMissLat,
  StCnt,
  StMissCnt,
  StMissLat,
  AmoCnt,
  AmoMissCnt,
  AmoMissLat
} L1PerfType deriving(Bits, Eq, FShow);

typedef enum {
  TlbAccessCnt,
  TlbMissCnt,
  TlbMissLat,
  TlbFlushCnt
} TlbPerfType deriving(Bits, Eq, FShow);

typedef enum {
  DecRedirectBr,
  DecRedirectJmp,
  DecRedirectJr,
  DecRedirectOther
} DecStagePerfType deriving(Bits, Eq, FShow);

typedef enum {
  SupRenameCnt, // number of cycles that commit user cnt > 1
  ExeRedirectBr,
  ExeRedirectJr,
  ExeRedirectOther,
  ExeKillLd,
  ExeTlbExcep,
  HtifStallCnt,
  HtifStallLat
} ExeStagePerfType deriving(Bits, Eq, FShow);

typedef enum {
  CycleCnt,
  InstCnt,
  UserInstCnt,
  SupComUserCnt, // number of cycles that commit user cnt > 1
  ComBrCnt,
  ComJmpCnt,
  ComJrCnt,
  ComRedirect,
  TrapCnt,
  SretCnt,
  MrtsCnt
} ComStagePerfType deriving(Bits, Eq, FShow); 

// PerfReq = XXPerfType

typedef struct {
  perfType pType;
  Bit#(64) data;
} PerfResp#(type perfType) deriving(Bits, Eq);

interface Perf#(type perfType);
  method Action setStatus(Bool doStats); // change whether we collect data
  method Action req(perfType r);
  method ActionValue#(PerfResp#(perfType)) resp;
  method Bool respValid;
endinterface

// query performance counters in the whole processor
typedef Bit#(4) PerfType; // for all XXPerfType

// which stage/module to query
typedef enum {
  ICache,
  DCache,
  ITlb,
  DTlb,
  DecStage,
  ExeStage,
  ComStage
} PerfLocation deriving(Bits, Eq, FShow);

typedef struct {
  PerfLocation loc;
  PerfType pType;
} ProcPerfReq deriving(Bits, Eq);

typedef struct {
  PerfLocation loc;
  PerfType pType;
  Bit#(64) data;
} ProcPerfResp deriving(Bits, Eq);
