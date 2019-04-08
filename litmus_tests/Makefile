TESTS = SB MP

BIN_DIR := bin
DUMP_DIR := dump

BINS = $(addprefix $(BIN_DIR)/,$(TESTS))
DUMPS = $(addsuffix .S,$(addprefix $(DUMP_DIR)/,$(TESTS)))

PREFIX = riscv64-unknown-linux-gnu-

CXX = $(PREFIX)g++
OBJDUMP = $(PREFIX)objdump

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

clean:
	rm -rf $(BIN_DIR) $(DUMP_DIR)

.DEFAULT_GOAL = all

.PHONY: all clean


