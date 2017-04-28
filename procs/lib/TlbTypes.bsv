
// Copyright (c) 2017 Massachusetts Institute of Technology
// 
// Permission is hereby granted, free of charge, to any person
// obtaining a copy of this software and associated documentation
// files (the "Software"), to deal in the Software without
// restriction, including without limitation the rights to use, copy,
// modify, merge, publish, distribute, sublicense, and/or sell copies
// of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
// 
// The above copyright notice and this permission notice shall be
// included in all copies or substantial portions of the Software.
// 
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
// EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
// MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
// NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
// BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
// ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
// CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.

import Types::*;
import ProcTypes::*;

// processor req/resp with I/D TLB
typedef struct{
    Addr  addr;
    Bool  write;
} TlbReq deriving(Eq, Bits, FShow);
typedef Tuple2#(Addr, Maybe#(Exception)) TlbResp;

// I/D TLB req type to L2 TLB
typedef enum {
    LdTranslation, // get missing translation from parent TLB
    SetDirtyOnly // just update dirty bit in PTE
} TlbRqToPType deriving(Bits, Eq, FShow);

// Only for Sv39
typedef 27 VpnSz;
typedef Bit#(27) Vpn;
typedef 38 PpnSz;
typedef Bit#(38) Ppn;
typedef 12 PageOffsetSz;
typedef Bit#(PageOffsetSz) PageOffset;
typedef 9 VpnISz;
typedef Bit#(VpnISz) VpnI;
typedef Bit#(2) PageSize;

typedef struct {
    Bit#(16) reserved;
    Bit#(20) ppn2;
    Bit#(9) ppn1;
    Bit#(9) ppn0;
    Bit#(3) reserved_sw;
    Bool d;
    Bool r;
    PTE_Type pte_type;
    Bool valid;
} PTE_Sv39 deriving(Bits, Eq, FShow);

typedef struct {
    Bool global;
    Bool s_r;
    Bool s_w;
    Bool s_x;
    Bool u_r;
    Bool u_w;
    Bool u_x;
} PTE_Type deriving (Bits, Eq, FShow);

function Bool is_leaf_pte_type(PTE_Type pte_type);
    return pte_type.s_r; // all leaf PTE are readable by supervisor
endfunction

// I don't choose to overload pack/unpack is to avoid PTE_Type being stored in pack mode
// Then TLB search will need to decode each PTE_Type from reg
function Bit#(4) pack_PTE_Type(PTE_Type x);
    Bit#(7) bitvec = {pack(x.global), pack(x.s_r), pack(x.s_w), pack(x.s_x), pack(x.u_r), pack(x.u_w), pack(x.u_x)};
    return (case (bitvec)
        7'b0000000: 0;
        7'b1000000: 1;
        7'b0100101: 2;
        7'b0110111: 3;
        7'b0100100: 4;
        7'b0110110: 5;
        7'b0101101: 6;
        7'b0111111: 7;
        7'b0100000: 8;
        7'b0110000: 9;
        7'b0101000: 10;
        7'b0111000: 11;
        7'b1100000: 12;
        7'b1110000: 13;
        7'b1101000: 14;
        7'b1111000: 15;
        default:    ?;
    endcase);
endfunction

function PTE_Type unpack_PTE_Type(Bit#(4) x);
    Bit#(7) bitvec = (case (x)
        0:  7'b0000000;
        1:  7'b1000000;
        2:  7'b0100101;
        3:  7'b0110111;
        4:  7'b0100100;
        5:  7'b0110110;
        6:  7'b0101101;
        7:  7'b0111111;
        8:  7'b0100000;
        9:  7'b0110000;
        10: 7'b0101000;
        11: 7'b0111000;
        12: 7'b1100000;
        13: 7'b1110000;
        14: 7'b1101000;
        15: 7'b1111000;
    endcase);
    return (PTE_Type {
        global: unpack(bitvec[6]),
        s_r:    unpack(bitvec[5]),
        s_w:    unpack(bitvec[4]),
        s_x:    unpack(bitvec[3]),
        u_r:    unpack(bitvec[2]),
        u_w:    unpack(bitvec[1]),
        u_x:    unpack(bitvec[0])
    });
endfunction

function Bit#(64) pack_PTE_Sv39(PTE_Sv39 x);
    return {x.reserved, x.ppn2, x.ppn1, x.ppn0, x.reserved_sw, pack(x.d), pack(x.r), pack_PTE_Type(x.pte_type), pack(x.valid)};
endfunction

function PTE_Sv39 unpack_PTE_Sv39(Bit#(64) x);
    return (PTE_Sv39 {
        reserved:     x[63:48],
        ppn2:         x[47:28],
        ppn1:         x[27:19],
        ppn0:         x[18:10],
        reserved_sw:  x[9:7],
        d:            unpack(x[6]),
        r:            unpack(x[5]),
        pte_type:     unpack_PTE_Type(x[4:1]),
        valid:        unpack(x[0])
    });
endfunction

function Bit#(64) setPteRef(Bit#(64) pte);
    pte[5] = 1;
    return pte;
endfunction

function Bit#(64) setPteRefDirty(Bit#(64) pte);
    pte[5] = 1;
    pte[6] = 1;
    return pte;
endfunction

// add new TLB entry to TLB array
typedef struct {
    Vpn      vpn;
    Ppn      ppn;
    PTE_Type page_perm;
    PageSize page_size;
    Asid     asid;
    Bool     dirty;
} TlbEntry deriving (Bits, Eq, FShow);

// SV39 translate
function Vpn getVpn(Addr addr) = addr[38:12];

function PageOffset getPageOffset(Addr addr) = truncate(addr);

function Addr translate(Addr addr, Ppn ppn, PageSize page_size);
    return zeroExtend(case (page_size)
        0:  {ppn, getPageOffset(addr)};       // normal page
        1:  {ppn[37:9], addr[20:0]};      // megapage
        2:  {ppn[37:18], addr[29:0]};     // gigapage
        default: {ppn, getPageOffset(addr)};  // this shouldn't happen
    endcase);
endfunction

function Vpn getMaskedVpn(Vpn vpn, PageSize sz);
    return (case (sz)
        0:  (vpn);
        1:  (vpn & 27'h7FFFE00); // megapage mask
        2:  (vpn & 27'h7FC0000); // gigapage mask
        3:  0;
    endcase);
endfunction

function Ppn getMaskedPpn(Ppn ppn, PageSize sz);
    return (case (sz)
        0:  (ppn);
        1:  (ppn & 38'h3FFFFFFE00); // megapage mask
        2:  (ppn & 38'h3FFFFC0000); // gigapage mask
        3:  0;
    endcase);
endfunction

function Bool hasSv39Permission(VMInfo vm_info, Bool inst_tlb, Bool write, PTE_Type pte_type);
    Bit#(2) prv_level = vm_info.prv;
    Bool has_permission = False;
    // in spike vm_info won't change during page walk
    // but it may change in our systgem
    // if it is no longer SV39, just return page fault
    if(vm_info.vm == vmSv39) begin
        if (prv_level == 0) begin
            // user
            has_permission = inst_tlb ? pte_type.u_x : (write ? pte_type.u_w : pte_type.u_r);
        end
        else if (prv_level == 1) begin
            // supervisor
            has_permission = inst_tlb ? pte_type.s_x : (write ? pte_type.s_w : pte_type.s_r);
        end
        // we ignore hypervisor for now
    end
    return has_permission;
endfunction
