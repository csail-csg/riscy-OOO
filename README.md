# RISC-V Out-of-Order Processors

This repository contains an OOO RISC-V processor written in Bluespec System Verilog (BSV).

## Getting Started

How to get started with this repository (should work on both Ubuntu 14.04 and 16.04):

- Get all the submodules.

        $ git submodule update --init --recursive

- Get dependencies for RISC-V toolchain and connectal.

        $ sudo apt-get install autoconf automake autotools-dev curl libmpc-dev libmpfr-dev libgmp-dev gawk build-essential bison flex texinfo gperf libtool patchutils bc zlib1g-dev device-tree-compiler pkg-config python-ply

- Build RISC-V tools

        $ cd tools
        $ ./build.sh 20 # build using 20 threads
        $ cd ..
        
    RISC-V tools will be builted to `tools/RV64G`.

- Setup environment variables for the project (`RISCY_HOME` should be the path to this repo).

        $ source ./setup.sh
        
- Get Verilator for simulation and connectal utilities for programming FPGA.
The version of Verilator in the Ubuntu package has a bug that prevents running our BSV designs.
We use a PPA to provide a newer version of Verilator.

        $ sudo apt-add-repository -y ppa:jamey-hicks/connectal
        $ sudo apt-get update
        $ sudo apt-get install verilator connectal

- Copy DDR3 IP from Bluespec installation (environment variable $BLUESPECDIR should have been set).

        $ cd fpgautils/xilinx/vc707/ddr3_1GB_bluespec
        $ ./copy_verilog.sh
        $ cd ../../../..

- Build BusyBox.
BusyBox is the first step of building Linux image.

        $ cd tools
        $ ./build-buxybox.sh 20 # build using 20 threads
        $ cd ..
        
    BusyBox will be at `tools/RV64G/busybox-1.21.1/busybox`.

- Build Linux image.
The file system of Linux is currently using initramfs.
Assume `$TEST_DIR` is the directory that we want to include in the Linux image.
We can build the image as follows:

        $ cd tools
        $ ./build-linux.py --testdir $TEST_DIR --jobs 20 # build using 20 threads
        $ cd ..
        
    The Linux image is included in the bbl, which is at `tools/RV64G/build-pk/bbl`.
    After Linux is booted, the contents of `$TEST_DIR` can be found in `/test`.
    If `--testdir` is not specified, then `/test` will be an empty folder after Linux boots.

## Simulation
- Build the OOO processor with `$N` cores for simulation.
If CORE_NUM is not specified, we build for 1 core by default.

        $ cd $RISCY_HOME/procs/RV64G_OOO
        $ make build.verilator CORE_NUM=$N -j20

    The build result will be `$RISCY_HOME/procs/build/RV64G_OOO.core_$N.check_deadlock/verilator/bin/ubuntu.exe`.

- Run tests in simulation (these are bare metal programs for 1 core).

        $ make run.verilator TEST=assembly
        $ make run.verilator TEST=benchmarks

## Boot Linux on AWS F1 FPGA
Simulation is too slow to boot Linux, so we boot Linux on FPGA.

On AWS, we compile the design on a C4 (c4.4xlarge) machine which runs the FPGA Developer AMI provided by AWS.
After compilation, we run the design on FPGA using an F1 (f1.2xlarge) machine.
As a result, this repo should be cloned to a place shared by C4 and F1.

In general, we build the hardware part of the design on C4, while we build the software part and run the design on F1.
(This is mainly because C4 and F1 use different operating systems.)
Therefore, the build of RISC-V tools should be done in the F1 machine.
In fact, only tools/riscv-fesvr needs to be built for compiling and running the design (see below).

Installation of Verilator (`sudo apt-get install verilator`) can be skipped if we are not using AWS for simulation.

Installation of connectal utilities (`sudo apt-get install connectal`) is not needed on C4 (but may be needed on F1).

### Setup C4
Most of setups in the "Getting Started" section are not needed on C4. Just `source setup.sh`.

The additional thing is to get the AWS HDK repo (https://github.com/aws/aws-fpga.git).
It should be located at `~/aws-fpga` (it could be a symlink pointing to where the repo truly resides).
We are using commit `e107da6487221a820a07ebd3b82de71c5362c313` of the HDK repo; we have not tested the lastest commit.
Here is an example way of setup.

    $ cd ~
    $ git clone https://github.com/aws/aws-fpga.git
    $ git checkout e107da6487221a820a07ebd3b82de71c5362c313 -b riscy-OOO

If you would like to get email notification when the FPGA compilation finishes, you can do the following.

    $ export EMAIL=<email address>
    $ export SNS_NOTIFY_EMAIL=$EMAIL

### Compilation of hardware on C4

- Compile the hardware part.
The following commands build the hardware for `$N` cores.
We need to pass in the path for device tree compiler to the makefile to help later build of software part (C4 does not have device tree compiler).

        $ cd $RISCY_HOME/procs/RV64G_OOO
        $ make gen.awsf1 CORE_NUM=$N DTC_PATH=/usr/bin/dtc
        $ cd ../build/RV64G_OOO.core_$N.check_deadlock/awsf1
        $ make bits -j16

    After the build finishes, we can find out the IDs of the FPGA image in `$RISCY_HOME/procs/build/RV64G_OOO.core_$N.check_deadlock/awsf1/build/scripts/fpga_image_ids.json`, i.e., FPGA image ID `afi-xxx` and FPGA image global ID `agfi-yyy`.

- Wait for the FPGA image to be available.
We run the following command on C4 to monitor the state of the FPGA image.

        $ aws ec2 describe-fpga-images --fpga-image-ids afi-xxx

    When the `State` field in the command output changes from `pending` to `available`, the FPGA image will be available and we can switch to F1 to run the design.

### Setup F1
Most of setups in the "Getting Started" section are not needed on C4. Just `source setup.sh`.

The additional thing is to build riscv-fesvr.

    $ cd $RISCY_HOME/tools
    $ ./build-fesvr.sh 8 # build using 8 threads

### Run on F1
- Finish compilation of software part.

        $ cd $RISCY_HOME/procs/build/RV64G_OOO.core_$N.check_deadlock/awsf1
        $ make exe
 
- Program FPGA.
 
        $ sudo fpga-load-local-image -S 0 -I agfi-yyy

- Run the design to boot Linux.
We need to copy the bbl (`tools/RV64G/build-pk/bbl`) to F1.
The following command boots Linux with 2GB memory.

        $ $RISCY_HOME/procs/build/RV64G_OOO.core_$N.check_deadlock/awsf1/bin/ubuntu.exe --core-num $N --mem-size 2048 --ignore-user-stucks 1000000 -- /path/to/bbl
    
    The processor detects potential deadlock by checking if a user level instruction has been executed during a period of time.
    This will output a lot of "deadlock" warnings when the processor is booting linux or idling in shell.
    To avoid such warnings, we use the `--ignore-user-stucks A_LARGE_NUMBER` option as shown above.
    This will suppress the first `A_LARGE_NUMBER` of user-level instruction deadlock messages.
    
    Hit `ctrl-c` when you want to exit.
    
    It should be noted that we need to program the FPGA before each run of the design (even if the design does not change).

### Other build configurations
`$RISCY_HOME/procs/RV64G_OOO/Makefile` contains several options to configure the build. For example, the makefile can be invoked in the following way to build for C4:

    $ cd $RISCY_HOME/procs/RV64G_OOO
    $ make gen.awsf1 CORE_NUM=$N DTC_PATH=/usr/bin/dtc CORE_SIZE=<SMALL/LARGE/...> CACHE_SIZE=<SMALL/LARGE/...> PERF_COUNT=<true/false> TSO_MM=<true/false> CHECK_DEADLOCK=<true/false> RENAME_DEBUG=<true/false> USER_CLK_PERIOD=<clock period in ns>

Below are the expanations for these options.
It should be noted that these options can also be applied when building for simulation.

- `CORE_SIZE`: the size of each core in the processor.
The detailed buffer sizes for each `CORE_SIZE` configuration are defined in `$RISCY_HOME/procs/RV64G_OOO/ProcConfig.bsv`.
Default value is `SMALL` (64-entry ROB).

- `CACHE_SIZE`: the size of caches in the processor.
The detailed parameters for each `CACHE_SIZE` configuration are defined in `$RISCY_HOME/procs/RV64G_OOO/ProcConfig.bsv`.
Default value is `LARGE`.

- `PERF_COUNT`: enable or disable the performance counters.
If set to `true`, performance counters will be implemetned in the processor; otherwise, performance counters will not be implemented and any query to performance counters will return zero.
Default is `true`.

- `TSO_MM`: enable TSO memory model or not.
If set to `true`, the processor implements TSO; otherwise, the processor implements a weak memroy model *WMM* (https://doi.org/10.1109/PACT.2017.29).
Default is `false`.

- `CHECK_DEADLOCK`: enable or disable the check on potential deadlock.
If set to `true`, the processor sends out a message to host software in case an instruction has been stuck at the ROB head for too long or a memory access has been stuck at cache MSHR for too long.
Otherwise, no such check will be performed.
Default is `true`.

- `RENAME_DEBUG`: enable or disable checks on register renaming.
If set to `true`, the commit stage of the processor will perform simple checks on the register renaming of the committing instruction, and sends a message to host software if the checks fail.
Otherwise, no such check will be performed.
Default is `true`.

- `USER_CLK_PERIOD`: the FPGA clock period for the processor in nano seconds.
The default value depends on the `CORE_SIZE` configuration.
It is recommended to make the clock period a multple of 8, because the AWS FPGA shell clock period is 8ns, and an async reset signal in our design is derived from the FPGA shell reset.
Doing so can prevent Xilinx Vivado from overconstraining the timing related to this async reset.

As an example, when we build the 4-core TSO multiprocessor on AWS, we invoke the makefile in the following way:

    $ cd $RISCY_HOME/procs/RV64G_OOO
    $ make gen.awsf1 CORE_NUM=4 DTC_PATH=/usr/bin/dtc CORE_SIZE=TINY CACHE_SIZE=MC PERF_COUNT=false TSO_MM=true CHECK_DEADLOCK=false RENAME_DEBUG=false USER_CLK_PERIOD=40

Since 4 OOO cores will make the FPGA pretty congested, we use the smallest core and cache configurations (`TINY` and `MC`, respectively), and we turn off the checkes for deadlock and renaming.
We also lower down the clock frequency to 25MHz (i.e., 40ns period).

## VC707 FPGA

- Build an `$N`-core processor for FPGA.
We are using Xilinx Vivado 2015.4 on Ubuntu 14.04 or Ubuntu 16.04.
VC707 shoud only be able to hold 1 core (i.e., `$N=1`).

        $ cd $RISCY_HOME/procs/RV64G_OOO
        $ make build.vc707 CORE_NUM=$N

    The build result will be in `$RISCY_HOME/procs/build/RV64G_OOO.core_$N.deadlock_check/vc707/bin`. The other build options can be passed to the makefile as in AWS.
    
- Boot Linux on FPGA.
Since VC707 board only has 1GB DRAM, we boot Linux with 1GB memory.

        $ $RISCY_HOME/procs/build/RV64G_OOO.core_$N.check_deadlock/vc707/bin/ubuntu.exe --core-num $N --mem-size 1024  --ignore-user-stucks 1000000 -- $RISCY_HOME/tools/RV64G/build-pk/bbl
 
     Hit `ctrl-c` when you want to exit.


