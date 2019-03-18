
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

// Core size and cache size are controlled by macros CORE_XXX and CACHE_XXX. In
// MICRO 2018 paper, configuration BASE-T is defining CORE_SMALL and
// CACHE_LARGE, and configuration BASE-R is defining CORE_MEDIUM and
// CACHE_LARGE. Due to resource restrictions, 4-core multiprocessor on AWS is
// defining CORE_TINY and CACHE_MC.

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

// tournament predictor, other options are: BHT, TOUR, GSELECT, GSHARE. NOTE
// that the predictors are of different size.
`define DIR_PRED_TOUR

`define LOG_DEADLOCK_CYCLES 26 // 64M cycles for deadlock detection

// Be lazy in enq to RS and ROB. LSQ is by default lazy-enq. 1-elem spec FIFOs
// (pipeline stage regs) are by default not lazy-enq.
`define RS_LAZY_ENQ True
`define ROB_LAZY_ENQ True

`define L1_TLB_SIZE 32 // L1 fully assoc TLB size

`define L2_TLB_HUGE_SIZE 8 // L2 2MB/1GB TLB size
`define LOG_L2_TLB_4KB_SIZE 10 // L2 4KB TLB log size (1024 entries)
`define LOG_L2_TLB_4KB_WAYS 2 // L2 4KB TLB log ways (4 ways)

// FMA bookkeeping FIFO: add 1 to allow simultaneous enq/deq
`define BOOKKEEPING_FP_FMA_SIZE TAdd#(`XILINX_FP_FMA_LATENCY, 1)
// INT MUL bookkeeping FIFO: add 1 to allow simultaneous enq/deq, another 1
// because of internal flow control in MUL unit
`define BOOKKEEPING_INT_MUL_SIZE TAdd#(`XILINX_INT_MUL_LATENCY, 2)

// non-blocking DTLB
`define DTLB_REQ_NUM 4
// non-blocking L2 TLB
`define L2TLB_REQ_NUM 2

`define DRAM_MAX_READS TExp#(`LOG_LLC_WAYS) // max reads in DRAM, match LLC ways
`define DRAM_MAX_WRITES 16 // write buffer size in AWS DRAM controller
`define DRAM_MAX_REQS 24
`define DRAM_LATENCY 120 // model a constant dram latency

// ROB and LSQ behave slightly different in case of in-order core
`define INORDER_CORE

//
// ==== CACHE SIZE ====
//

`ifdef CACHE_LARGE

    // L1
    `define LOG_L1_LINES 9 // 32KB
    `define LOG_L1_WAYS 3 // 8 ways

    // LLC
    `define LOG_LLC_LINES 14 // 1MB
    `define LOG_LLC_WAYS 4 // 16 ways

`endif

`ifdef CACHE_MC_1MB

    // L1
    `define LOG_L1_LINES 9 // 32KB
    `define LOG_L1_WAYS 2 // 4 ways

    // LLC
    `define LOG_LLC_LINES 14 // 1MB
    `define LOG_LLC_WAYS 4 // 16 ways

`endif

`ifdef CACHE_MC_2MB

    // L1
    `define LOG_L1_LINES 9 // 32KB
    `define LOG_L1_WAYS 2 // 4 ways

    // LLC
    `define LOG_LLC_LINES 15 // 2MB
    `define LOG_LLC_WAYS 4 // 16 ways

`endif

//
// ==== CORE SIZE ====
//

`ifdef CORE_INORD

    // superscalar
    `define sizeSup 2

    // ROB
    `define ROB_SIZE 32

    // speculation
    `define NUM_EPOCHS 6
    `define NUM_SPEC_TAGS 6

    // LSQ
    `define LDQ_SIZE 12
    `define STQ_SIZE 7
    `define SB_SIZE 2

    // reservation station sizes
    `define RS_ALU_SIZE 8
    `define RS_MEM_SIZE 8 
    `define RS_FPUMULDIV_SIZE 8

`endif

//
// ==== derived parameters ====
//


