
# Copyright (c) 2017 Massachusetts Institute of Technology
# 
# Permission is hereby granted, free of charge, to any person
# obtaining a copy of this software and associated documentation
# files (the "Software"), to deal in the Software without
# restriction, including without limitation the rights to use, copy,
# modify, merge, publish, distribute, sublicense, and/or sell copies
# of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
# 
# The above copyright notice and this permission notice shall be
# included in all copies or substantial portions of the Software.
# 
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
# EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
# MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
# NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
# BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
# ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
# CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

CONNECTALDIR := $(RISCY_HOME)/connectal

PROC := RV64G_MultiCycle

# FPGA is congested, use a slow clk...
USER_CLK_PERIOD ?= 25
# always include perf counter
PERF_COUNT := true
# dram type in simulation: VC707 or AWSF1
SIM_DRAM_TYPE := AWSF1
# use Xilinx FPU IP cores
USE_XILINX_FPU := true

# default 1 core (multicore does not work now)
CORE_NUM := 1
# default check cache deadlock
CHECK_DEADLOCK ?= true

# directories
BUILD_DIR := $(RISCY_HOME)/procs/build/$(PROC).core_$(CORE_NUM)
ifeq ($(CHECK_DEADLOCK),true)
BUILD_DIR := $(BUILD_DIR).check_deadlock
endif

PROJECTDIR = $(BUILD_DIR)/$(BOARD)

PROC_DIR := $(RISCY_HOME)/procs
PROC_LIB_DIR := $(PROC_DIR)/lib
PROC_CPP_DIR := $(PROC_DIR)/cpp
PROC_BSV_DIR := $(PROC_DIR)/$(PROC)

COHERENCE_DIR := $(RISCY_HOME)/coherence/src

FPGAUTILS_DIR := $(RISCY_HOME)/fpgautils
FPGA_LIB_DIR := $(FPGAUTILS_DIR)/lib
XILINX_IP_DIR := $(FPGAUTILS_DIR)/xilinx
CORE_SCRIPT_DIR := $(FPGAUTILS_DIR)/core-scripts

# interfaces
S2H_INTERFACES = ProcRequest:ProcDmaWrapper.procReq \
				 HostDmaRequest:ProcDmaWrapper.hostDmaReq \
				 DeadlockRequest:ProcDmaWrapper.deadlockReq
H2S_INTERFACES = ProcDmaWrapper:ProcIndication,HostDmaIndication,DeadlockIndication,RenameDebugIndication:host

BSVFILES = $(PROC_LIB_DIR)/ProcIF.bsv \
		   $(PROC_LIB_DIR)/HostDmaIF.bsv \
		   $(PROC_LIB_DIR)/DeadlockIF.bsv \
		   $(PROC_LIB_DIR)/RenameDebugIF.bsv \
		   $(PROC_LIB_DIR)/VerificationPacket.bsv \
		   $(PROC_LIB_DIR)/Performance.bsv

CPPFILES = $(PROC_CPP_DIR)/testproc.cpp \
		   $(PROC_CPP_DIR)/htif_riscy.cpp \
		   $(PROC_CPP_DIR)/proc_ind.cpp \
		   $(PROC_CPP_DIR)/host_dma.cpp \
		   $(PROC_CPP_DIR)/to_host.cpp \
		   $(PROC_CPP_DIR)/print_buff.cpp

CONNECTALFLAGS += -D IMPORT_HOSTIF -D XILINX_SYS_CLK --nocache -v
CONNECTALFLAGS += --bscflags=" -D USER_CLK_PERIOD=$(USER_CLK_PERIOD) "
CONNECTALFLAGS += --bsvpath=$(FPGA_LIB_DIR) --bsvpath=$(PROC_LIB_DIR) --bsvpath=$(PROC_BSV_DIR) --bsvpath=$(COHERENCE_DIR)
CONNECTALFLAGS += --verilog $(XILINX_IP_DIR)/reset_regs --verilog $(XILINX_IP_DIR)/fpu
CONNECTALFLAGS += --cflags=" -std=c++0x "
CONNECTALFLAGS += --clibdir=$(RISCY_TOOLS)/lib --clib=riscv --clib=fesvr
CONNECTALFLAGS += -I$(RISCY_TOOLS)/include

# always use xilinx sync fifo
CONNECTALFLAGS += --bscflags " -D USE_XILINX_SYNC_FIFO "

# core num
CONNECTALFLAGS += --bscflags=" -D NUM_CORES=$(CORE_NUM) "

# perf counter
ifeq ($(PERF_COUNT),true)
CONNECTALFLAGS += --bscflags=" -D PERF_COUNT "
endif

# check deadlock
ifeq ($(CHECK_DEADLOCK),true)
CONNECTALFLAGS += --bscflags=" -D CHECK_DEADLOCK "
endif

# FPU implementation
ifeq ($(USE_XILINX_FPU),true)
CONNECTALFLAGS += --bscflags=" -D USE_XILINX_FPU "
endif

CONNECTALFLAGS += --bscflags=" +RTS -K1G -RTS " --bscflags=" -steps-max-intervals 200 "
# I use dynamicAssert in bsv
CONNECTALFLAGS += --bscflags=" -check-assert "
# suppress "duplicate folder" and "unfolding over ... steps" warnings
CONNECTALFLAGS += --bscflags=" -suppress-warnings S0073:G0024 "
# verilator flags 
VERILATOR_DEFINES = VM_PARALLEL_BUILDS=1 VERILATOR_PROJECT_ARGS="-output-split 10000"


ifneq (,$(filter $(BOARD),vc707 awsf1))
# synthesize for vc707 or awsf1

AUTOTOP = --interface pins:ProcDmaWrapper.pins

# sync FIFOs
SYNC_FIFO_XCI = $(CONNECTALDIR)/out/$(BOARD)/sync_fifo_w32_d16/sync_fifo_w32_d16.xci
SYNC_BRAM_FIFO_XCI = $(CONNECTALDIR)/out/$(BOARD)/sync_bram_fifo_w36_d512/sync_bram_fifo_w36_d512.xci

CONNECTALFLAGS += --xci $(SYNC_FIFO_XCI) --xci $(SYNC_BRAM_FIFO_XCI)

prebuild:: $(SYNC_FIFO_XCI) $(SYNC_BRAM_FIFO_XCI)

$(SYNC_FIFO_XCI): $(CORE_SCRIPT_DIR)/synth_sync_fifo.tcl
	(cd $(PROJECTDIR); vivado -mode batch -source $^)

$(SYNC_BRAM_FIFO_XCI): $(CORE_SCRIPT_DIR)/synth_sync_bram_fifo.tcl
	(cd $(PROJECTDIR); vivado -mode batch -source $^)

# AWS needs to know sync fifo is used
ifeq ($(BOARD),awsf1)
CONNECTALFLAGS += -D AWSF1_SYNC_FIFO
endif

# VC707 needs some more clocking constraints
ifeq ($(BOARD),vc707)
CONNECTALFLAGS += -C $(XILINX_IP_DIR)/vc707/constraints/clocks.xdc
endif

# cross clock domain for creating user reset (TODO AWS)
ifeq ($(BOARD),vc707)
CONNECTALFLAGS += -C $(RISCY_HOME)/procs/constraints/vc707_user_reset.xdc
else
endif

# DRAM
ifeq ($(BOARD),vc707)
PIN_TYPE = DDR3_1GB_Pins
PIN_TYPE_INCLUDE = DDR3Common
CONNECTALFLAGS += --bscflags " -D USE_VC707_DRAM " \
				  --verilog $(XILINX_IP_DIR)/vc707/ddr3_1GB_bluespec/ \
				  -C $(XILINX_IP_DIR)/vc707/constraints/ddr3_1GB_bluespec.xdc
else
PIN_TYPE = AWSDramPins
PIN_TYPE_INCLUDE = AWSDramCommon
CONNECTALFLAGS += -D AWSF1_DDR_A
CONNECTALFLAGS += --bscflags " -D USE_AWSF1_DRAM "
endif

# FPU
FP_FMA_XCI = $(CONNECTALDIR)/out/$(BOARD)/fp_fma/fp_fma.xci
FP_DIV_XCI = $(CONNECTALDIR)/out/$(BOARD)/fp_div/fp_div.xci
FP_SQRT_XCI = $(CONNECTALDIR)/out/$(BOARD)/fp_sqrt/fp_sqrt.xci

# also configure the latency and rate (number of cycles per input) of IP core
FP_FMA_LATENCY = 4
FP_DIV_LATENCY = 12
FP_DIV_RATE = 1
FP_SQRT_LATENCY = 8
FP_SQRT_RATE = 1

CONNECTALFLAGS += --xci $(FP_FMA_XCI) --xci $(FP_DIV_XCI) --xci $(FP_SQRT_XCI)

prebuild:: $(FP_FMA_XCI) $(FP_DIV_XCI) $(FP_SQRT_XCI)

$(FP_FMA_XCI): $(CORE_SCRIPT_DIR)/synth_fp_fma.tcl
	(cd $(PROJECTDIR); vivado -mode batch -source $^ -tclargs $(FP_FMA_LATENCY))

$(FP_DIV_XCI): $(CORE_SCRIPT_DIR)/synth_fp_div.tcl
	(cd $(PROJECTDIR); vivado -mode batch -source $^ -tclargs $(FP_DIV_LATENCY) $(FP_DIV_RATE))

$(FP_SQRT_XCI): $(CORE_SCRIPT_DIR)/synth_fp_sqrt.tcl
	(cd $(PROJECTDIR); vivado -mode batch -source $^ -tclargs $(FP_SQRT_LATENCY) $(FP_SQRT_RATE))

# Force to generate IP core, because we pass args to synth tcl
.PHONY: $(FP_FMA_XCI) $(FP_DIV_XCI) $(FP_SQRT_XCI)

# AWS needs to know xilinx FPU is used
ifeq ($(BOARD),awsf1)
CONNECTALFLAGS += -D AWSF1_FPU
endif

else
# simulation
# define BSIM to use simulation version of DRAM and enable assert
CONNECTALFLAGS += --bscflags " -D BSIM " --cflags " -D BSIM " \
				  --bscflags " -D USE_$(SIM_DRAM_TYPE)_DRAM "

endif

include $(CONNECTALDIR)/Makefile.connectal

clean:
	@echo "Use make clean.boardname"

clean.%:
	rm -rf $(BUILD_DIR)/$*

# run tests
TEST ?= assembly

run:
	@echo "Use make run.boardname"

run.%:
	./run_test.py -e $(BUILD_DIR)/$*/bin/ubuntu.exe -t $(TEST)

