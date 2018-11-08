#pragma once

#define BOOT_ROM_BASE   0x00001000
#define MEM_LOADER_BASE 0x01000000
#define CLINT_BASE      0x02000000
#define CLINT_SIZE      0x000c0000
#define MSIP_BASE       CLINT_BASE
#define MAIN_MEM_BASE   0x80000000

// CLINT offset 0 is the first MSIP

// MEM_LOADER_BASE is to input the DRAM starting addr to copy elf
// MEM_LOADER_BASE+8 is to signal busy (1 means busy, so copy not done yet)
