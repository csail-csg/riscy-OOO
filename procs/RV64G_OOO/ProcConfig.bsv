
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
`define f True
`define d True

//`define NUM_CORES 1 // defined in make file

// Set this define to use the FMA for Add and Mul
`define REUSE_FMA

`define sizeSup 2 // 2 way superscalar

// speculation
`define DIR_PRED_GSELECT // branch predictor, other options are: BHT, TOUR
`define NUM_EPOCHS 16
`define NUM_SPEC_TAGS 16

// ROB
`define SUP_ROB
`define ROB_SIZE 48 // match BOOM
`define PHYS_REG_COUNT TAdd#(64,`ROB_SIZE)

// L1 TLB
`define TLB_SIZE 32

// LSQ
`define LSQ_VTAG
`define LDSTQ_SIZE 16 // match BOOM
`define SB_SIZE 4 // [sizhuo] make SB smaller, probably easier for synth

// D$
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

// performance counters
//`define PERF_COUNT -- defined in makefile

// Debugging
//`define VERIFICATION_PACKETS

// maximum pending reads in DRAM
`define DRAMLLC_MAX_READS 8 // match LLC MSHR size

// deadlock check
`define LOG_DEADLOCK_CYCLES 26 // 64M cycles for deadlock detection
