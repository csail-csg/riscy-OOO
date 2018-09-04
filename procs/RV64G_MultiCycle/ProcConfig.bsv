
// Copyright (c) 2018 Massachusetts Institute of Technology
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

`define m True
`define a True
`define f True
`define d True

// Set this define to use the FMA for Add and Mul
`define REUSE_FMA

//`define NUM_CORES -- defined in makefile

`define sizeSup 1
`define TLB_SIZE 32
`define LOG_DCACHE_BANKS 0 // 1 bank for D$ (4 misses)
`define LOG_BOOT_ROM_BYTES 12 // 4KB boot rom

// tandem verification
//`define VERIFICATION_PACKETS

// PERF_COUNT and CHECK_DEADLOCK are defined in makefile, because they are also
// used in other repos (e.g., coherence) which do not include this file

// common settings
`define DRAMLLC_MAX_READS 8 // match LLC MSHR size
`define LOG_DEADLOCK_CYCLES 26 // 64M cycles for deadlock detection

`define PHYS_REG_COUNT TAdd#(64,`ROB_SIZE)

// These are just defined to make things compile, these are not really used
`define NUM_EPOCHS 2
`define NUM_SPEC_TAGS 2
`define ROB_SIZE 2
`define LDSTQ_SIZE 2 // match BOOM
`define SB_SIZE 2
`define SUP_ROB
`define LSQ_VTAG
`define DIR_PRED_GSELECT
