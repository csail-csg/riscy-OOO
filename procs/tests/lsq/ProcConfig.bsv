// macros to just make things compile
`define rv64 True
`define m True
`define a True
`define f True
`define d True
`define sizeSup 1
`define NUM_CORES 1
`define NUM_EPOCHS 1
`define DRAMLLC_MAX_READS 1
`define LOG_DEADLOCK_CYCLES 26
`define LOG_BOOT_ROM_BYTES 12

// XXX these are the relevant macros
`define ROB_SIZE 10000 // ROB index as test id
`define LDQ_SIZE 16
`define STQ_SIZE 12
`define SB_SIZE 4
`define NUM_SPEC_TAGS TAdd#(`LDQ_SIZE, `STQ_SIZE)

