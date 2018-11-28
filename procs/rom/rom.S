# NUM_CORES should be defined in gcc flag

#include "mmio.h"

.text

    .global _start
_start:
    csrr t0, mhartid
    beqz t0, hart_zero # hart 0
    j hart_non_zero # hart > 0

    .align 2
ipi_handler:
    # hart > 0 is being waked up by hart 0
    # clear my mip.msip
    li t0, MSIP_BASE
    csrr t1, mhartid
    slli t1, t1, 2
    add t0, t0, t1
    sw x0, 0(t0)
    fence.i
    fence # make sure the store has been performed
    # reset mtvec, mstatus, mie
    csrw mtvec, x0
    csrw mie, x0
    csrw mstatus, x0
    # now start booting
    j boot

hart_zero:
    # request mem loader to initialize DRAM (i.e. copy elf)
    li t0, MEM_LOADER_BASE
    li t1, MAIN_MEM_BASE
    sw t1, 0(t0)
    # wait for mem loader to finish
    fence # make sure mem loader has seen the store before we check
1:  ld t1, 8(t0)
    bnez t1, 1b

    # after loading mem, clear cycle/inst, stop future inst from speculation
    fence.i
    fence
    csrw mcycle, x0
    csrw minstret, x0

    # wakeup other harts via ipi
    # t0: value to write to other harts' mip.msip
    li t0, 1
    # t1: base mip.msip addr
    li t1, MSIP_BASE
    # t2: offset to each hart's mip.msip
    li t2, ((NUM_CORES - 1) * 4)
1:  beqz t2, boot
    add t3, t1, t2
    sw t0, 0(t3)
    addi t2, t2, -4
    j 1b

    # finish waking up other harts, start booting
    j boot

hart_non_zero:
    # hart > 0 wait for hart 0 to wake it up
    # setup mtvec to handle ipi at ipi_handler (low 2 bits of ipi_hander is
    # already 0)
    la t0, ipi_handler
    csrw mtvec, t0
    # turn on mie.msie and then mstatus.mie
    csrsi mie, 8
    csrsi mstatus, 8
    # wait for ipi to wake me up
1:  wfi
    j 1b
    
boot:
    la a1, _end # device tree follows the end of this
    csrr a0, mhartid
    li t0, MAIN_MEM_BASE
    jr t0
