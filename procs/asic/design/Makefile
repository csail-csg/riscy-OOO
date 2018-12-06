
SIZE ?= MEDIUM
SECURITY ?= false

BUILD_DIR := ./build_$(SIZE)
VERILOG_DIR := ./verilog_$(SIZE)

ifeq ($(SECURITY),true)
BUILD_DIR := $(BUILD_DIR)_security
VERILOG_DIR := $(VERILOG_DIR)_security
endif

EXT_PATH = -p +:$(RISCY_HOME)/procs/lib \
		   -p +:$(RISCY_HOME)/procs/RV64G_OOO \
		   -p +:$(RISCY_HOME)/coherence/src

OPT_FLAGS = -unspecified-to X \
			-opt-undetermined-vals

BSV_DEF = -D CORE_$(SIZE) \
		  -D CACHE_LARGE \
		  -D NUM_CORES=1 \
		  -D XILINX_FP_FMA_LATENCY=3 \
		  -D XILINX_INT_MUL_LATENCY=2
# use Xilinx FPU so we can override FPU
BSV_DEF += -D USE_XILINX_FPU

# security
ifeq ($(SECURITY),true)
BSV_DEF += -D SECURITY
endif

all:
	mkdir -p $(BUILD_DIR)
	mkdir -p $(VERILOG_DIR) 
	bsc -u \
		-bdir $(BUILD_DIR) \
		-info-dir $(BUILD_DIR) \
		-simdir $(BUILD_DIR) \
		-vdir $(VERILOG_DIR) \
		-aggressive-conditions \
		-check-assert \
		-suppress-warnings S0073:G0024 \
		+RTS -K1G -RTS \
		-steps-max-intervals 200 \
		$(EXT_PATH) \
		$(OPT_FLAGS) \
		$(BSV_DEF) \
		-verilog CoreTop.bsv

clean:
	rm -rf $(BUILD_DIR) $(VERILOG_DIR)
