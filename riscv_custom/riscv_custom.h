#pragma once

#ifdef __riscv

#define riscv_roi_begin() asm volatile("csrwi 0x801, 1")
#define riscv_roi_end() asm volatile("csrwi 0x801, 0")
#define riscv_terminate() asm volatile("csrwi 0x800, 0")

#else

#define riscv_roi_begin()
#define riscv_roi_end()
#define riscv_terminate()

#endif
