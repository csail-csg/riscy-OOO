CONNECTALDIR := $(RISCV_VENDOR)/connectal

PROC := RV64G_OoO

# FPGA is congested, use a slow clk...
USER_CLK_PERIOD := 50

CORE_NUM ?= 1
DIR_PRED ?= gselect
CHECK_DEADLOCK ?= true
LSQ_VTAG ?= true
SUP_ROB ?= true
WIRE_MSHR ?= false
TLB_SIZE ?= 32
PERF_COUNT ?= true
# sync fifo type can be: normal, bsv, connectal, xilinx
SYNC_FIFO_TYPE ?= xilinx

# directories
BUILD_DIR := $(RISCV_BUILD)/procs/$(PROC).core_$(CORE_NUM).sync_$(SYNC_FIFO_TYPE).$(USER_CLK_PERIOD)ns.tlb_$(TLB_SIZE).$(DIR_PRED)
ifeq ($(SUP_ROB),true)
BUILD_DIR := $(BUILD_DIR).sup_rob
endif
ifeq ($(LSQ_VTAG),true)
BUILD_DIR := $(BUILD_DIR).lsq_vtag
endif
ifeq ($(WIRE_MSHR),true)
BUILD_DIR := $(BUILD_DIR).wire_mshr
endif
ifeq ($(CHECK_DEADLOCK),true)
BUILD_DIR := $(BUILD_DIR).check_deadlock
endif
ifeq ($(PERF_COUNT),true)
BUILD_DIR := $(BUILD_DIR).perf
endif

PROJECTDIR = $(BUILD_DIR)/$(BOARD)

PROC_DIR := $(RISCV_HOME)/procs
PROC_LIB_DIR := $(PROC_DIR)/lib
PROC_CPP_DIR := $(PROC_DIR)/cpp
PROC_BSV_DIR := $(PROC_DIR)/$(PROC)

COHERENCE_DIR := $(RISCV_HOME)/coherence/src_new

MULTIFPGA_DIR := $(RISCV_HOME)/multifpga
DDR3_LIB_DIR := $(MULTIFPGA_DIR)/lib
XILINX_IP_DIR := $(MULTIFPGA_DIR)/xilinx
CORE_SCRIPT_DIR := $(MULTIFPGA_DIR)/core-scripts

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
CONNECTALFLAGS += --bsvpath=$(DDR3_LIB_DIR) --bsvpath=$(PROC_LIB_DIR) --bsvpath=$(PROC_BSV_DIR) --bsvpath=$(COHERENCE_DIR)
CONNECTALFLAGS += --verilog $(XILINX_IP_DIR)/reset_regs
CONNECTALFLAGS += --cflags=" -std=c++0x "
CONNECTALFLAGS += --clibdir=$(RISCV_TOOLS)/lib --clib=riscv --clib=fesvr
CONNECTALFLAGS += -I$(RISCV_TOOLS)/include/spike -I$(RISCV_TOOLS)/include

# core num
ifeq ($(CORE_NUM),1)
CONNECTALFLAGS += --bscflags=" -D SINGLE_CORE "
else
ifeq ($(CORE_NUM),2)
CONNECTALFLAGS += --bscflags=" -D DUAL_CORE "
else
$(error CORE_NUM can only be 1 or 2)
endif
endif

# branch predictor
dirPredType := $(shell echo $(DIR_PRED) | tr a-z A-Z)
CONNECTALFLAGS += --bscflags=" -D DIR_PRED_$(dirPredType) "

# tlb size
CONNECTALFLAGS += --bscflags=" -D TLB_SIZE=$(TLB_SIZE) "

# perf counter
ifeq ($(PERF_COUNT),true)
CONNECTALFLAGS += --bscflags=" -D PERF_COUNT "
endif

# sync fifo type
ifeq ($(SYNC_FIFO_TYPE),bsv)
CONNECTALFLAGS += --bscflags " -D USE_BSV_BRAM_SYNC_FIFO "
endif
ifeq ($(SYNC_FIFO_TYPE),connectal)
CONNECTALFLAGS += --bscflags " -D USE_CONNECTAL_BRAM_SYNC_FIFO "
endif
ifeq ($(SYNC_FIFO_TYPE),xilinx)
CONNECTALFLAGS += --bscflags " -D USE_XILINX_SYNC_FIFO "
endif

# use superscalar ROB
ifeq ($(SUP_ROB),true)
CONNECTALFLAGS += --bscflags=" -D SUP_ROB "
endif

# use virtual tags for LSQ
ifeq ($(LSQ_VTAG),true)
CONNECTALFLAGS += --bscflags=" -D LSQ_VTAG "
endif

# use wire-based MSHR (otherwise use EHR-based MSHR)
ifeq ($(WIRE_MSHR),true)
CONNECTALFLAGS += --bscflags=" -D UNSAFE_L1_CRQ_MSHR -D UNSAFE_L1_PRQ_MSHR -D UNSAFE_LL_CRQ_MSHR "
endif

# check deadlock
ifeq ($(CHECK_DEADLOCK),true)
CONNECTALFLAGS += --bscflags=" -D CHECK_DEADLOCK "
endif

CONNECTALFLAGS += --bscflags=" +RTS -K1G -RTS " --bscflags=" -steps-max-intervals 200 "
# I use dynamicAssert in bsv
CONNECTALFLAGS += --bscflags=" -check-assert "
# suppress "duplicate folder" and "unfolding over ... steps" warnings
CONNECTALFLAGS += --bscflags=" -suppress-warnings S0073:G0024 "
# something just for zynq (harmless to keep it...)
CONNECTALFLAGS += -D USE_ACP
VERILATOR_DEFINES = VM_PARALLEL_BUILDS=1 VERILATOR_PROJECT_ARGS="-output-split 10000"

#CONNECTALFLAGS += --bsimsource=$(PROC_LIB_DIR)/BluesimSqrt.c


ifneq (,$(filter $(BOARD),vc707 vc707g2))
# synthesize for vc707 or vc707g2

# ddr3
PIN_TYPE = DDR3TopPins
PIN_TYPE_INCLUDE = DDR3TopPins
AUTOTOP = --interface pins:ProcDmaWrapper.pins
CONNECTALFLAGS += --verilog $(XILINX_IP_DIR)/ddr3_bluespec/

# constraints
ifeq ($(BOARD),vc707)
CONNECTALFLAGS += -C $(XILINX_IP_DIR)/constraints/clocks.xdc \
				  -C $(XILINX_IP_DIR)/constraints/ddr3_bluespec.xdc \
				  -C $(PROC_DIR)/constraints/user_clocks.xdc \
				  -C $(PROC_DIR)/constraints/no_poweropt.xdc
else
CONNECTALFLAGS += -C $(XILINX_IP_DIR)/constraints/ddr3_bluespec_vc707g2.xdc \
				  -C $(PROC_DIR)/constraints/user_clocks_vc707g2.xdc \
				  -C $(PROC_DIR)/constraints/no_poweropt.xdc
endif

# sync fifo IP
SYNC_FIFO_XCI = $(CONNECTALDIR)/out/$(BOARD)/sync_fifo_w32_d16/sync_fifo_w32_d16.xci
SYNC_BRAM_FIFO_XCI = $(CONNECTALDIR)/out/$(BOARD)/sync_bram_fifo_w36_d512/sync_bram_fifo_w36_d512.xci

prebuild:: $(SYNC_FIFO_XCI) $(SYNC_BRAM_FIFO_XCI)

$(SYNC_FIFO_XCI): $(CORE_SCRIPT_DIR)/synth_sync_fifo.tcl
	(cd $(PROJECTDIR); vivado -mode batch -source $^)

$(SYNC_BRAM_FIFO_XCI): $(CORE_SCRIPT_DIR)/synth_sync_bram_fifo.tcl
	(cd $(PROJECTDIR); vivado -mode batch -source $^)

CONNECTALFLAGS += --xci $(SYNC_FIFO_XCI) \
				  --xci $(SYNC_BRAM_FIFO_XCI)

endif


ifneq (,$(filter $(BOARD),bluesim verilator))
# simulation
# define BSIM to use simulation version of DDR3 ctrl
# and enable debug assertions
CONNECTALFLAGS += --bscflags " -D BSIM " --cflags " -D BSIM "

endif

include $(CONNECTALDIR)/Makefile.connectal

run.%:
	@$(MAKE) -C $(RISCV_HOME)/programs/$(PROG) --no-print-directory run PROC_BUILD_DIR=$(BUILD_DIR)/$*
	
clean.%:
	rm -rf $(BUILD_DIR)/$*

clean:
	@echo "Use make clean.boardname"


