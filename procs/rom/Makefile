
RV_GCC = riscv64-unknown-elf-gcc \
		 -march=rv64g \
		 -mabi=lp64 \
		 -static \
		 -mcmodel=medany \
		 -fvisibility=hidden \
		 -nostdlib \
		 -nostartfiles \
		 -I../cpp
RV_OBJDUMP = riscv64-unknown-elf-objdump \
			 --disassemble-all \
			 --disassemble-zeroes

CORES = 1 2 4 8

OUTDIR = out
BINS = $(addprefix $(OUTDIR)/rom_core_,$(CORES))
DUMPS = $(addsuffix .dump,$(BINS))

all: $(BITS) $(DUMPS)

$(BINS): | $(OUTDIR)

$(OUTDIR):
	mkdir $(OUTDIR)

$(BINS): $(OUTDIR)/rom_core_%: rom.S link.ld ../cpp/mmio.h
	$(RV_GCC) -DNUM_CORES=$* -T link.ld rom.S -o $@

$(DUMPS): $(OUTDIR)/rom_core_%.dump: $(OUTDIR)/rom_core_%
	$(RV_OBJDUMP) $< > $@

clean:
	rm -rf $(OUTDIR)

.PHONY: all clean

.DEFAULT_GOAL := all
