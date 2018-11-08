OUTPUT_ARCH( "riscv" )
ENTRY(_start)

SECTIONS
{
  . = 0x00001000;
  .text : { *(.text) }
  . = ALIGN(0x8);
  _end = .;
}

