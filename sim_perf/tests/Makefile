TESTS = hello \
		ptr_chase

SRC_DIR = .
BUILD_DIR = ../build/tests

BINS = $(addprefix $(BUILD_DIR)/,$(addsuffix .riscv,$(TESTS)))
DUMPS = $(addprefix $(BUILD_DIR)/,$(addsuffix .dump,$(TESTS)))
RUNS = $(addprefix run.,$(TESTS))

HOOK_DIR = ../../riscv_custom
HOOK_HEADER = $(HOOK_DIR)/riscv_custom.h

RISCV_GCC = riscv64-unknown-elf-gcc
RISCV_GCC_OPTS = -I$(HOOK_DIR) -O3 -static
RISCV_DUMP = riscv64-unknown-elf-objdump -d

# simulation load latency
LAT ?= 4
SIM = ../build/sim
RISCV_PK = $(RISCY_TOOLS)/riscv64-unknown-elf/bin/pk

$(DUMPS): %.dump: %.riscv
	$(RISCV_DUMP) $< > $@

$(RUNS): run.%: $(BUILD_DIR)/%.riscv
	$(SIM) BW --lat $(LAT) --out $(BUILD_DIR)/$*_$(LAT).out -- $(RISCV_PK) $<

define compile_template
$$(BUILD_DIR)/$(1).riscv: $$(SRC_DIR)/$(1).cpp $$(HOOK_HEADER) | $$(BUILD_DIR)
	$$(RISCV_GCC) $$(RISCV_GCC_OPTS) -o $$@ $$<
endef

$(foreach test,$(TESTS),$(eval $(call compile_template,$(test))))

$(BUILD_DIR):
	mkdir -p $@

clean:
	rm -rf $(BUILD_DIR)

all: $(BINS) $(DUMPS)

run: $(RUNS)

.DEFAULT_GOAL := all

.PHONY: $(RUNS) all run clean
