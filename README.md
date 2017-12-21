# Riscy OOO Processors

This repository contains an OOO RISC-V processor written in Bluespec System Verilog (BSV).

## Getting Started

How to get started with this repository (tested in Ubuntu 14.04):

1. Get all the submodules.

        $ git submodule update --init --recursive

2. Get dependencies for building the RISC-V toolchain and building using connectal.

        $ sudo apt-get install autoconf automake autotools-dev curl libmpc-dev libmpfr-dev libgmp-dev gawk build-essential bison flex texinfo gperf libtool patchutils bc python-ply

3. Build RISC-V tools.
`build.sh` builds the tools into `tools/RV64G`.

        $ cd tools
        $ ./build.sh
        $ cd ..

4. Setup environment variables for the Riscy project (`RISCY_HOME` should be the path to this riscy-OOO repo).

        $ source ./setup.sh

5. Get Verilator for simulation and connectal utilities for programming FPGA.
The version of Verilator in the Ubuntu package has a bug that prevents running our BSV designs.
We use a PPA to provide a newer version of Verilator.

        $ sudo apt-add-repository -y ppa:jamey-hicks/connectal
        $ sudo apt-get update
        $ sudo apt-get install verilator connectal

6. Copy DDR3 IP from Bluespec installation.

        $ cd fpgautils/xilinx/vc707/ddr3_1GB_bluespec
        $ ./copy_verilog.sh
        $ cd ../../../..

7. Build Linux (optional).

        $ cd tools/riscv-linux
        $ make ARCH=riscv defconfig
        $ make ARCH=riscv vmlinux
        
    Please refer to https://github.com/riscv/riscv-tools/tree/priv-1.7#-the-linuxrisc-v-installation-manual for instructions on building the disk image for Linux.

## Simulation
1. Build the OOO processor for simulation.

        $ cd $RISCY_HOME/procs/RV64G_OOO
        $ make build.verilator -j20

    The build result will be in `$RISCY_HOME/procs/build/RV64G_OOO.core_1.check_deadlock/verilator/bin`.

2. Run tests in simulation.

        $ make run.verilator TEST=assembly
        $ make run.verilator TEST=benchmarks

## VC707 FPGA
1. Build the OOO processor for FPGA.
We are using Xilinx Vivado 2015.4 on Ubuntu 14.04.

        $ cd $RISCY_HOME/procs/RV64G_OOO
        $ make build.vc707

    The build result will be in `$RISCY_HOME/procs/build/RV64G_OOO.core_1.check_deadlock/vc707/bin`.
    
2. Boot Linux on FPGA.

        $ $RISCY_HOME/procs/build/RV64G_OOO.core_1.check_deadlock/vc707/bin/ubuntu.exe --just-run --mem-size 1024 -- +ramdisk=/path/to/disk/image $RISCY_TOOLS/bin/bbl $RISCY_HOME/tools/riscv-linux/vmliux
 
     Hit `ctrl-c` when you want to exit.
  
## AWS F1 FPGA
We compile the design on a C4 (c4.4xlarge) machine which runs the FPGA Developer AMI provided by AWS.
After compilation, we run the design on FPGA using an F1 (f1.2xlarge) machine.
As a result, this repo should be cloned to a place shared by C4 and F1.

It should be noted that the build of RISC-V tools should be done in the F1 machine.
This is because the design (the host software part) will use shared libraries built from the RISC-V tools when it is being run on the FPGA.
(We can also build the RISC-V in another Ubuntu machine with same version as F1, and copy the libraries `$RISCY_TOOLS/lib` and headers `$RISCY_TOOLS/include` to AWS.)

`sudo apt-get install verilator connectal` can be skipped, because AWS has provided ways to program the FPGA and  it is unlikely that we want to use the F1 machine for simulation.

### Compilation on C4
1. Setup.
The AWS HDK repo (https://github.com/aws/aws-fpga.git) should be located at `~/aws-fpga` (it could be a symlink pointing to where the repo truly resides).
We are using commit `e107da6487221a820a07ebd3b82de71c5362c313` of the HDK repo; we have not tested the lastest commit.
Here is an example way of setup.

        $ cd ~
        $ git clone https://github.com/aws/aws-fpga.git
        $ git checkout e107da6487221a820a07ebd3b82de71c5362c313 -b riscy-OOO

    If you would like to get email notification when the FPGA compilation finishes, you can do the following.
    
        $ export EMAIL=<email address>
        $ export SNS_NOTIFY_EMAIL=$EMAIL

2. Compile.
As noted before, the host software part of the design will use shared libraries built in F1.
Since C4 is running an different OS from F1, we skip the step of building host software here, and leave it until we get to run the design on F1.
Here we only build the FPGA image.

        $ cd $RISCY_HOME/procs/RV64G_OOO
        $ make gen.awsf1
        $ cd ../build/RV64G_OOO.core_1.check_deadlock/awsf1
        $ make bits -j16

    After the build finishes, we can find out the IDs of the FPGA image in `$RISCY_HOME/procs/build/RV64G_OOO.core_1.check_deadlock/awsf1/build/scripts/fpga_image_ids.json`, i.e., FPGA image ID `afi-xxx` and FPGA image global ID `agfi-yyy`.

3. Wait for the FPGA image to be available to F1.
We run the following command to monitor the state of the FPGA image.

        $ aws ec2 describe-fpga-images --fpga-image-ids afi-xxx

    When the `State` field in the command output changes from `pending` to `available`, the FPGA image will be available on F1 and we can switch to F1 to run the design.

### Run on F1
1. Finish compilation.
We finish the compilation by building the host software part of the design.

        $ cd $RISCY_HOME/procs/RV64G_OOO
        $ make gen.awsf1
        $ cd ../build/RV64G_OOO.core_1.check_deadlock/awsf1
        $ make exe
 
2. Program FPGA.
 
        $ sudo fpga-load-local-image -S 0 -I agfi-yyy

3. Run the design to boot Linux.

        $ $RISCY_HOME/procs/build/RV64G_OOO.core_1.check_deadlock/vc707/bin/ubuntu.exe --just-run --mem-size 1024 -- +ramdisk=/path/to/disk/image $RISCY_TOOLS/bin/bbl $RISCY_HOME/tools/riscv-linux/vmliux

    Hit `ctrl-c` when you want to exit.

It should be noted that we need to program the FPGA before each run of the design.