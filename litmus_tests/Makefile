TESTS = SB MP MP_addr

BIN_DIR := bin
DUMP_DIR := dump

BINS = $(addprefix $(BIN_DIR)/,$(TESTS))
DUMPS = $(addsuffix .S,$(addprefix $(DUMP_DIR)/,$(TESTS)))

PREFIX = riscv64-unknown-linux-gnu-

CXX = $(PREFIX)g++
OBJDUMP = $(PREFIX)objdump

LINUX_JOBS ?= 16

all: $(BINS) $(DUMPS)


$(BINS): $(BIN_DIR)/%: %.cpp
	$(CXX) -static -O3 -std=c++11 -o $@ $^ -pthread

$(DUMPS): $(DUMP_DIR)/%.S: $(BIN_DIR)/%
	$(OBJDUMP) -d $^ > $@

$(BINS): | $(BIN_DIR)

$(DUMPS): | $(DUMP_DIR)

$(BIN_DIR):
	mkdir -p $@

$(DUMP_DIR):
	mkdir -p $@

linux:
	$(RISCY_HOME)/tools/build-linux.py --testdir $(BIN_DIR) --jobs $(LINUX_JOBS)
	cp $(RISCY_TOOLS)/build-pk/bbl .

clean:
	rm -rf $(BIN_DIR) $(DUMP_DIR)

.DEFAULT_GOAL = all

.PHONY: all clean


