
# Copyright (c) 2018 Massachusetts Institute of Technology
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

PROC := RV64G_MultiCycle

# FPGA is congested, use a slow clk...
USER_CLK_PERIOD ?= 24
# always include perf counter
PERF_COUNT := true
# dram type in simulation: VC707 or AWSF1
SIM_DRAM_TYPE := AWSF1
# use Xilinx FPU IP cores
USE_XILINX_FPU ?= true

# default 1 core
CORE_NUM ?= 1
# default check cache deadlock
CHECK_DEADLOCK ?= true

# directories
BUILD_DIR := $(RISCY_HOME)/procs/build/$(PROC).core_$(CORE_NUM)
ifeq ($(CHECK_DEADLOCK),true)
BUILD_DIR := $(BUILD_DIR).check_deadlock
endif

include $(RISCY_HOME)/procs/scripts/Makefile.common

