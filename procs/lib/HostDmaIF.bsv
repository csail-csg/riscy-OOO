
// currently each DMA access is blocking, so there is no tag in req/resp

// DMA ifc is similar to AXI, addr should be aligned w.r.t dword

typedef enum {HostDmaRead, HostDmaWrite} HostDmaCmd deriving(Bits, Eq, FShow);

typedef 8 LogMaxHostBurstLen;

interface HostDmaRequest;
    method Action req(HostDmaCmd cmd, Bit#(32) addr, Bit#(LogMaxHostBurstLen) burstLenMinusOne);
    method Action wrData(Bit#(64) data, Bit#(8) byteEn, Bool last);
endinterface

interface HostDmaIndication;
    method Action rdData(Bit#(64) data, Bit#(LogMaxHostBurstLen) id);
    method Action wrDone;
    // FPGA DRAM status
    method Action dramErr(Bit#(8) err);
    method Action dramStatus(Bool init);
endinterface
