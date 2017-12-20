
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

`define rv64 True
`define m True
`define a True

// Set this define to use the FMA for Add and Mul
`define REUSE_FMA

// Defines to match spike's behavior
// `define CYCLE_COUNT_EQ_INST_COUNT
`define DISABLE_STIP
`define LOOK_LIKE_A_ROCKET

// Workarounds
// `define WORKAROUND_ISSUE_27
// `define SERIALIZE_MEM_REQS
`define FLUSH_CACHES_ON_HTIF

// flush cache when sending out inter proc interrupt
`define FLUSH_CACHES_ON_IPI

// [sizhuo] if set isa.f = isa.d = False, then need to recompile gcc/pk/bbl/linux without FPU
// because bbl use macro __riscv_hard_float to determine FPU existence, and __riscv_hard_float is a macro in gcc
// I have difficulty fitting the design into zc706, so I just drop FPU
// Fortunately linux booting doesn't really use FPU
//`define USE_DUMMY_FPU // in dual core, we don't have space for FPU

`ifdef SINGLE_CORE
    // single core settings
    `define NUM_CORES 1

    `define f True
    `define d True

    `define sizeSup 2

    `define NUM_EPOCHS 16 //8
    `define NUM_SPEC_TAGS 16
    `define ROB_SIZE 48 // match BOOM
    //`define TLB_SIZE 8 //4 -- this is defined in makefile
    `define LDSTQ_SIZE 16 // match BOOM
    `define SB_SIZE 4 //8 [sizhuo] make SB smaller, probably easier for synth
    `define LOG_DCACHE_BANKS 0 // 1 bank for D$ (4 misses)
    // reservation station sizes
    `define RS_ALU_SIZE 12
    `define RS_MEM_SIZE 12
    `define RS_FPUMULDIV_SIZE 8 // arbitrary number
    // book keeping fifo sizes
    `define BOOKKEEPING_MEM_SIZE 2 // TLB has 1 cycle latency
    `define BOOKKEEPING_FPUMULDIV_SIZE 4 // FPU has 2-elem FIFO, MULDIV has 2-elem FIFO
    // be lazy in reservation station wake and phy reg file
    `define LAZY_RS_RF True
    // be lazy in enq (LSQ is fixed to be lazy; bookkeeping fifos are small so not lazy)
    `define RS_LAZY_ENQ True
    `define ROB_LAZY_ENQ True

    // Debugging infrastructure
    //`define VERIFICATION_PACKETS
    // performance counters
    //`define PERF_COUNT -- this is defined in makefile
`endif

`ifdef DUAL_CORE
    // dual core settings
    `define NUM_CORES 2

    `define f False
    `define d False

    //`define f True
    //`define d True
    //`define USE_DUMMY_FPU

    `define sizeSup 1

    `define NUM_EPOCHS 8
    `define NUM_SPEC_TAGS 8
    `define ROB_SIZE 8
    //`define TLB_SIZE 4 -- this is defined in makefile
    `define LDSTQ_SIZE 4
    `define SB_SIZE 2
    `define LOG_DCACHE_BANKS 0
    // reservation station sizes
    `define RS_ALU_SIZE 8
    `define RS_MEM_SIZE 4 // match LSQ size
    `define RS_FPUMULDIV_SIZE 4 // not expecting FPU/MUL/DIV num > ROB_SIZE / 2
    // book keeping fifo sizes
    `define BOOKKEEPING_MEM_SIZE 2 // TLB has 1 cycle latency
    `define BOOKKEEPING_FPUMULDIV_SIZE 4 // not expecting FPU/MUL/DIV num > ROB_SIZE / 2
    // be lazy in reservation station wake and phy reg file
    `define LAZY_RS_RF True
    // be lazy in enq
    `define RS_LAZY_ENQ True
    `define ROB_LAZY_ENQ True

    //`define VERIFICATION_PACKETS // in dual core, don't do verifycation...
`endif

// common settings
`define DRAMLLC_MAX_READS 8 // match LLC MSHR size
`define LOG_DEADLOCK_CYCLES 26 // 64M cycles for deadlock detection

`define PHYS_REG_COUNT TAdd#(64,`ROB_SIZE)
