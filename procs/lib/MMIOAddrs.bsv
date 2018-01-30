import Types::*;
import ProcTypes::*;
import CCTypes::*;

// data aligned addr
typedef TSub#(AddrSz, LgDataSzBytes) DataAlignedAddrSz;
typedef Bit#(DataAlignedAddrSz) DataAlignedAddr;

function DataAlignedAddr getDataAlignedAddr(Addr a) = truncateLSB(a);

// base addr for each MMIO reg/device (aligned to Data)
DataAlignedAddr bootRomBaseAddr  = getDataAlignedAddr(64'h00001000);
DataAlignedAddr msipBaseAddr     = getDataAlignedAddr(64'h02000000);
DataAlignedAddr mtimecmpBaseAddr = getDataAlignedAddr(64'h02004000);
DataAlignedAddr mtimeBaseAddr    = getDataAlignedAddr(64'h0200bff8);
DataAlignedAddr mainMemBaseAddr  = getDataAlignedAddr(64'h80000000);

// XXX Each msip reg is 32-bit, while mtime and each mtimecmp are 64-bit. We
// assume Data is 64-bit. We hard code this relation in all MMIO logic.

// upper bound addr (bound itself is invalid addr) for each MMIO reg/device
// (aligned to Data)
DataAlignedAddr bootRomBoundAddr  = bootRomBaseAddr +
                                    fromInteger(valueof(TExp#(LgBootRomSzData)));
DataAlignedAddr msipBoundAddr     = msipBaseAddr +
                                    fromInteger(valueof(TDiv#(CoreNum, 2)));
DataAlignedAddr mtimecmpBoundAddr = mtimecmpBaseAddr +
                                    fromInteger(valueof(CoreNum));

// offset within each MMIO reg/device (aligned to Data)
typedef Bit#(TLog#(TDiv#(CoreNum, 2))) MSIPDataAlignedOffset;
typedef CoreId MTimCmpDataAlignedOffset;
