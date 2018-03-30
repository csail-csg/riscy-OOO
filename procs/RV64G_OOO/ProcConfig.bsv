
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

//
// ==== common parameters ====
//

`define rv64 True
`define m True
`define a True
`define f True
`define d True

//`define NUM_CORES 1 // defined in make file

//`define PERF_COUNT // defined in makefile

`define REUSE_FMA // use FMA for add and mul

`define LOG_BOOT_ROM_BYTES 12 // 4KB boot rom

`define DIR_PRED_TOUR // branch predictor, other options are: BHT, TOUR

`define LOG_DEADLOCK_CYCLES 26 // 64M cycles for deadlock detection

// Be lazy in reservation station wake and phy reg file, and enqs. LSQ is by
// default lazy. 1-elem spec FIFOs (pipeline stage regs) are by default not
// lazy.
`define LAZY_RS_RF True
`define RS_LAZY_ENQ True
`define ROB_LAZY_ENQ True

`define TLB_SIZE 32 // L1 TLB size

`define BOOKKEEPING_MEM_SIZE 2 // TLB has 1 cycle latency

// FMA bookkeeping FIFO: add 1 to allow simultaneous enq/deq
`define BOOKKEEPING_FP_FMA_SIZE TAdd#(`XILINX_FP_FMA_LATENCY, 1)
// INT MUL bookkeeping FIFO: add 1 to allow simultaneous enq/deq, another 1
// because of internal flow control in MUL unit
`define BOOKKEEPING_INT_MUL_SIZE TAdd#(`XILINX_INT_MUL_LATENCY, 2)

`define LOG_L1_WAYS 3 // 8 ways
`define LOG_LLC_WAYS 4 // 16 ways

`define DRAMLLC_MAX_READS TExp#(`LOG_LLC_WAYS) // max reads in DRAM, match LLC ways

//
// ==== CORE SIZE ====
//

`ifdef CORE_TINY

    // superscalar
    `define sizeSup 2

    // ROB
    `define ROB_SIZE 48

    // speculation
    `define NUM_EPOCHS 16
    `define NUM_SPEC_TAGS 32

    // LSQ
    `define LDQ_SIZE 18
    `define STQ_SIZE 11
    `define SB_SIZE 4

    // reservation station sizes
    `define RS_ALU_SIZE 12
    `define RS_MEM_SIZE 12
    `define RS_FPUMULDIV_SIZE 12

`endif

`ifdef CORE_SMALL

    // superscalar
    `define sizeSup 2

    // ROB
    `define ROB_SIZE 64

    // speculation
    `define NUM_EPOCHS 16
    `define NUM_SPEC_TAGS 32

    // LSQ
    `define LDQ_SIZE 24
    `define STQ_SIZE 14
    `define SB_SIZE 4

    // reservation station sizes
    `define RS_ALU_SIZE 16
    `define RS_MEM_SIZE 16
    `define RS_FPUMULDIV_SIZE 16

`endif

`ifdef CORE_SMALL_WIDE

    // superscalar
    `define sizeSup 4

    // ROB
    `define ROB_SIZE 64

    // speculation
    `define NUM_EPOCHS 16
    `define NUM_SPEC_TAGS 32

    // LSQ
    `define LDQ_SIZE 24
    `define STQ_SIZE 14
    `define SB_SIZE 4

    // reservation station sizes
    `define RS_ALU_SIZE 8
    `define RS_MEM_SIZE 8
    `define RS_FPUMULDIV_SIZE 16

`endif

`ifdef CORE_LARGE

    // superscalar
    `define sizeSup 2

    // ROB
    `define ROB_SIZE 128

    // speculation
    `define NUM_EPOCHS 16
    `define NUM_SPEC_TAGS 64

    // LSQ
    `define LDQ_SIZE 48
    `define STQ_SIZE 28
    `define SB_SIZE 4

    // reservation station sizes
    `define RS_ALU_SIZE 32
    `define RS_MEM_SIZE 32
    `define RS_FPUMULDIV_SIZE 32

`endif

`ifdef CORE_LARGE

    // superscalar
    `define sizeSup 4

    // ROB
    `define ROB_SIZE 128

    // speculation
    `define NUM_EPOCHS 16
    `define NUM_SPEC_TAGS 64

    // LSQ
    `define LDQ_SIZE 48
    `define STQ_SIZE 28
    `define SB_SIZE 4

    // reservation station sizes
    `define RS_ALU_SIZE 16
    `define RS_MEM_SIZE 16
    `define RS_FPUMULDIV_SIZE 32

`endif

//`ifdef CORE_MEDIUM
//
//    // ROB
//    `define ROB_SIZE 96
//
//    // speculation
//    `define NUM_EPOCHS 32
//    `define NUM_SPEC_TAGS 32
//
//    // LSQ
//    `define LDQ_SIZE 36
//    `define STQ_SIZE 21
//    `define SB_SIZE 4
//
//    // reservation station sizes
//    `define RS_ALU_SIZE 24
//    `define RS_MEM_SIZE 24
//    `define RS_FPUMULDIV_SIZE 24
//
//`endif

//
// ==== derived parameters ====
//


