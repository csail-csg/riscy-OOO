# Riscy OOO Processors

This repository contains an OOO RISC-V processor written in Bluespec System Verilog (BSV).

## Getting Started

How to get started with this repository (tested in Ubuntu 14.04):

1. Get all the submodules.

        $ git submodule update --init --recursive

2. Get dependencies for building the RISC-V toolchain and building using connectal.

        $ sudo apt-get install autoconf automake autotools-dev curl libmpc-dev libmpfr-dev libgmp-dev gawk build-essential bison flex texinfo gperf libtool patchutils bc python-ply connectal

3. Build RISC-V tools.
`build.sh` builds the tools into `tools/RV64G`.

        $ cd tools
        $ ./build.sh
        $ cd ..

4. Setup environment variables for the Riscy project (`RISCY_HOME` should be the path to this risy-OOO repo).

        $ source ./setup.sh

5. Get a newer version of Verilator.
The version of Verilator in the Ubuntu package has a bug that prevents running our BSV designs.
We use a PPA to provide a newer version of Verilator.

        $ sudo apt-add-repository -y ppa:jamey-hicks/connectal
        $ sudo apt-get update
        $ sudo apt-get install verilator

6. Copy DDR3 IP from Bluespec installation.

        $ cd fpgautils/xilinx/vc707/ddr3_1GB_bluespec
        $ ./copy_verilog.sh
        $ cd ../../../..

7. Build Linux (optional).

        $ cd tools/riscv-linux
        $ make ARCH=riscv defconfig
        $ make ARCH=riscv vmlinux
        
    Refer to https://github.com/riscv/riscv-tools/tree/priv-1.7#-the-linuxrisc-v-installation-manual for instructions on building the disk image for Linux.

## Simulation
1. Build the OOO processor for simulation. Build result is in `$RISCY_HOME/procs/build/RV64G_OOO.core_1.check_deadlock/verilator/bin`.

        $ cd procs/RV64G_OOO
        $ make build.verilator -j20

2. Run tests in simulation

        $ make run.verilator TEST=asembly
        $ make run.verilator TEST=benchmarks

## FPGA
1. Build the OOO processor for FPGA (Xilinx Vivado should have been set up). BUild result is in `$RISCY_HOME/procs/build/RV64G_OOO.core_1.check_deadlock/vc707/bin`.

        $ cd procs/RV64G_OOO
        $ make build.vc707

2. Boot Linux on FPGA

        $ $RISCY_HOME/procs/build/RV64G_OOO.core_1.check_deadlock/vc707/bin/ubuntu.exe --just-run --mem-size 1024 -- +ramdisk=/path/to/disk/image $RISCY_TOOLS/bin/bbl $RISCY_HOME/tools/riscv-linux/vmliux
        
 
