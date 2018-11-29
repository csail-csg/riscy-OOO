`include "ProcConfig.bsv"
import StmtFSM::*;
import Randomizable::*;
import Assert::*;
import Types::*;
import ProcTypes::*;
import CCTypes::*;

typedef `LOG_LLC_LINES LgLLLineNum;
typedef `LOG_LLC_WAYS LgLLWayNum;
typedef TExp#(LgLLWayNum) LLWayNum;
typedef 0 LgLLBankNum;
typedef TSub#(LgLLLineNum, TAdd#(LgLLWayNum, LgLLBankNum)) LgLLSetNum;
typedef LgLLSetNum LLIndexSz;

typedef `LOG_DRAM_REGION_NUM LgDramRegionNum;
typedef `LOG_DRAM_REGION_SIZE LgDramRegionSz;
typedef TAdd#(TAdd#(LLIndexSz, LgLLBankNum), LgLineSzBytes) LLIndexBankOffsetSz;

function Addr secureRotateAddr(Addr addr);
    // low bits: index + bank id + line offset without region
    Bit#(TSub#(LLIndexBankOffsetSz, LgDramRegionNum)) low = truncate(addr);
    // swap bits: to be swapped with dram region
    Bit#(LgDramRegionNum) swap = truncate(addr >> (valueof(LLIndexBankOffsetSz) - valueof(LgDramRegionNum)));
    // middle bits between swap and region
    Bit#(TSub#(LgDramRegionSz, LLIndexBankOffsetSz)) mid = truncate(addr >> valueof(LLIndexBankOffsetSz));
    // dram region
    Bit#(LgDramRegionNum) region = truncate(addr >> valueof(LgDramRegionSz));
    // high bits beyond phy mem boundary
    Bit#(TSub#(AddrSz, TAdd#(LgDramRegionNum, LgDramRegionSz))) high = truncateLSB(addr);
    return {high, swap, mid, region, low};
endfunction

function Addr refRotateAddr(Addr addr);
    Addr newAddr = addr;
    newAddr[30:25] = addr[15:10];
    newAddr[15:10] = addr[30:25];
    return newAddr;
endfunction

(* synthesize *)
module mkTb(Empty);
    staticAssert(`LOG_DRAM_REGION_NUM == 6, "64 regions");
    staticAssert(`LOG_DRAM_REGION_SIZE == 25, "32MB region");
    staticAssert(`LOG_LLC_LINES == 14, "1MB LLC");
    staticAssert(`LOG_LLC_WAYS == 4, "16-way LLC");

    Reg#(Bit#(16)) i <- mkReg(0);

    Randomize#(Bit#(31)) randPhyAddr <- mkGenericRandomizer;
    Reg#(Bit#(31)) phyAddr <- mkReg(0);

    Randomize#(Bit#(33)) randHighAddr <- mkGenericRandomizer;
    Bit#(33) fixedHighAddr = 33'h1DEADBEEF;

    Stmt test = seq
        randPhyAddr.cntrl.init;
        randHighAddr.cntrl.init;

        // random test
        while (i < 100) action
            let pa <- randPhyAddr.next;
            let ha <- randHighAddr.next;
            let addr = {ha, pa};
            let refAddr = refRotateAddr(addr);
            let newAddr = secureRotateAddr(addr);
            $display(fshow(addr), " -> ", fshow(newAddr), ", should be ", fshow(refAddr));
            doAssert(refAddr == newAddr, "wrong");
            i <= i + 1;
        endaction

        while(True) seq
            action
                if (phyAddr[23:0] == 0) $display("Testing ", fshow(phyAddr));

                let high <- randHighAddr.next;
                Addr addr = {high, phyAddr};
                let refAddr = refRotateAddr(addr);
                //$display("Testing ", fshow(addr), " -> ", fshow(refAddr));
                let newAddr = secureRotateAddr(addr);
                //$display("Impl: ", fshow(newAddr));
                doAssert(newAddr == refAddr, "rand wrong");

                addr = {fixedHighAddr, phyAddr};
                refAddr = refRotateAddr(addr);
                //$display("Testing ", fshow(addr), " -> ", fshow(refAddr));
                newAddr = secureRotateAddr(addr);
                //$display("Impl: ", fshow(newAddr));
                doAssert(newAddr == refAddr, "fixed wrong");

                phyAddr <= phyAddr + 1;
            endaction
            if (phyAddr == 0) break;
        endseq

        $display("PASS");
    endseq;

    mkAutoFSM(test);
endmodule
