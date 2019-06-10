# RiscyOO: RISC-V Out-of-Order Processors

This repository contains the RiscyOO processor written in Bluespec System Verilog (BSV).
RiscyOO implements the RISC-V 64-bit instruction set with the IMAFD extensions, i.e., RV64G.
It is a out-of-order superscalar cache-coherent multiprocessor which can boot multicore Linux and AWS F1 FPGA.
The design document for the processor can be found at https://github.com/csail-csg/RiscyOO_design_doc.

## Getting Started on a Local Ubuntu Machine

How to get started with this repository (should work on both Ubuntu 14.04 and 16.04):

- Install the Bluespec compiler (`bsc`).

        $ cd <place you want to put the bluespec compiler>
        $ wget http://www.bluespec.com/downloads/Bluespec-2016.07.beta1.tar.gz
        $ tar -xzf Bluespec-2016.07.beta1.tar.gz

    This will create a folder called `Bluespec-2016.07.beta1` in the current directory.
    We need to set up some environment variables for Bluespec:
    
        $ export BSPATH=/path/to/Bluespec-2016.07.beta1
        $ export BLUESPECDIR=$BSPATH/lib
        $ export PATH=$BSPATH/bin:$PATH
        $ export LM_LICENSE_FILE=<your bluespec license>
        
    The Bluespec compiler is not an open-source software, but academia users can get free licences by contacting Bluespec, Inc. (https://bluespec.com/).

    The Bluespec compiler uses the shared library `libgmp.so.3`, but Ubuntu does not provide this version of the library. To fix this, we can just creat a link for `libgmp.so.10`:
    
        $ cd /usr/lib/x86_64-linux-gnu # the folder containing libgmp.so, this is the path for ubuntu; the path may be different for other OS
        $ sudo ln -s libgmp.so.10 libgmp.so.3

- Get dependencies for RISC-V toolchain and connectal.

        $ sudo apt-get install autoconf automake autotools-dev curl libmpc-dev libmpfr-dev libgmp-dev gawk build-essential bison flex texinfo gperf libtool patchutils bc zlib1g-dev device-tree-compiler pkg-config python-ply

- Clone this `riscy-OOO` repo and get all the submodules.

        $ cd <place you want to put this repo>
        $ git clone https://github.com/csail-csg/riscy-OOO.git
        $ cd riscy-OOO
        $ git submodule update --init --recursive

- Build RISC-V tools

        $ cd tools
        $ ./build.sh 20 # build using 20 threads
        $ cd ..
        
    RISC-V tools will be builted to `tools/RV64G`.

- Setup environment variables for the project.

        $ source ./setup.sh
        
    `$RISCY_HOME` should be the path to this repo, and we will use `$RISCY_HOME` to refer to the path of this repo in the following.
        
- Get Verilator for simulation and connectal utilities for programming FPGA.
The version of Verilator in the Ubuntu package lacks certain features, so we use a PPA to provide a newer version of Verilator.

        $ sudo apt-add-repository -y ppa:jamey-hicks/connectal
        $ sudo apt-get update
        $ sudo apt-get install verilator connectal

- Copy DDR3 IP from Bluespec installation (environment variable `$BLUESPECDIR` should have been set).
This step is needed only if we are going to run the processor on VC707 FPGA.
Currently we recommond using AWS FPGA which does not require this step.

        $ cd $RISCY_HOME/fpgautils/xilinx/vc707/ddr3_1GB_bluespec
        $ ./copy_verilog.sh

- Build BusyBox.
BusyBox is the first step of building Linux image.

        $ cd $RISCY_HOME/tools
        $ ./build-buxybox.sh 20 # build using 20 threads
        
    BusyBox will be at `tools/RV64G/busybox-1.21.1/busybox`.

- Build a simple program used to shutdown the processor.
This program is always installed to the Linux image built using our script (the next step).

        $ cd $RISCY_HOME/riscv_custom/terminate
        $ make

- Build Linux image (bbl).
The file system of Linux is currently using initramfs.
Assume `$TEST_DIR` is the directory that we want to include in the Linux image.
We can build the image as follows:

        $ cd $RISCY_HOME/tools
        $ ./build-linux.py --testdir $TEST_DIR --jobs 20 # build using 20 threads
        
    The Linux image is included in the bbl, which is at `$RISCY_HOME/tools/RV64G/build-pk/bbl`.
    After Linux is booted, the contents of `$TEST_DIR` can be found in `/test`.
    If `--testdir` is not specified, then `/test` will be an empty folder after Linux boots.
    
    We currently configure Linux to support maximum 8 CPUs.
    (We can only fit 4 OOO cores on FPGA.)
    Change `$RISCY_HOME/tools/configs/linux_config` to support more CPUs (the upper bound should be 32).
    
    We have put some prebuilt Linux images (bbls) containing the [PARSEC](https://parsec.cs.princeton.edu/) benchmarks in `tools/images`.
    The sources files of PARSEC benchmarks that we are using can be found at https://github.com/csail-csg/parsec.
    We have also cross-compiled the [GAP](http://gap.cs.berkeley.edu/benchmark.html) benchmarks, which can be found at https://github.com/csail-csg/gapbs.
    Unfortunately, we cannot release the prebuilt images or the source code of SPEC benchmarks due to license issues.
        
- Build boot rom.
The processor uses a small boot rom to load the Linux image to its memory.
We can build the boot rom as follows:

        $ cd $RISCY_HOME/procs/rom
        $ make

    The boot-rom executables are `$RISCY_HOM/procs/rom/out/rom_core_<X>`, where `<X>` is the number of cores in the processor.
    That is, booting a single-core processor should use `rom_core_1`, while booting a 4-core processor should use `rom_core_4`.

## Simulation on a Local Ubuntu Machine
- Build the OOO processor with `$N` cores for simulation.
If CORE_NUM is not specified, we build for 1 core by default.

        $ cd $RISCY_HOME/procs/RV64G_OOO
        $ make build.verilator CORE_NUM=$N -j20

    The build result will be an executable `$RISCY_HOME/procs/build/RV64G_OOO.core_$N.core_SMALL.cache_LARGE.weak.l1_cache_lru.check_deadlock/verilator/bin/ubuntu.exe`.
    Strings after `core_$N`in the folder name reflects the processor configurations: core size is `SMALL`, cache size is `LARGE`, and the memory model is a weak model.
    Refer to the [Other build configurations](#other-build-configurations) section for the meaning of these configurations.

- Run tests in simulation. These are bare metal programs for 1 cores, so we can just use `$N=1` in the previous step.

        $ make run.verilator TEST=assembly
        $ make run.verilator TEST=benchmarks
        
    Assembly tests will output all zero for performance counters, while benchmark tests will output meaningful performance counter values.
    Source codes for assembly tests can be found at `$RISCY_HOME/tools/riscv-tests/isa/rv64ui`, while source codes for benchmark tests can be found at `$RISCY_HOME/tools/riscv-tests/benchmarks`.
    There are tons of debugging outputs generated during simulation, but they are currently redirected to `/dev/null`.
    See `$RISCY_HOME/scripts/run_test.py` about changing the redirection.

## Boot Linux on AWS F1 FPGA
Simulation is too slow to boot Linux, so we boot Linux on FPGA.
It should be noted that we cross-compile benchmark programs to RISC-V and build Linux images on our local machines instead of on AWS.
On AWS, we compile/synthesize the processor and run it on FPGA.

On AWS, we compile (and synthesize) the design on a C4 (e.g., c4.4xlarge) machine which runs the *FPGA Developer AMI* (our AMI version is 1.5.0) provided by AWS.
After compilation, we run the design on FPGA using an F1 (e.g., f1.2xlarge) machine.
As a result, this repo should be cloned to a place shared by C4 and F1.
We are using Amazon EFS to share files between C4 and F1 machines.

In general, we build the hardware part of the design on C4, while we build the software part and run the design on F1.
(This is mainly because C4 and F1 use different operating systems.)
Therefore, the build of RISC-V tools should be done in the F1 machine.
In fact, only tools/riscv-fesvr needs to be built for compiling and running the design.

### Setup C4
Most of the setups in the [Getting Started on a Local Ubuntu Machine](#getting-started-on-a-local-ubuntu-machine) section are not needed on C4.
Here are the steps to setup C4.

- Install the Bluepsec compiler.
(libgmp should be at `/usr/lib64`.)

- Install dependencies:

        $ sudo yum check-update
        $ sudo yum install -y python34 python34-pip
        $ sudo python -m pip install ply
        $ sudo python3 -m pip install boto3
        $ sudo python3 -m pip install requests

- Setup shared file system with F1 (e.g., using EFS).

- Get the AWS HDK repo (https://github.com/aws/aws-fpga).
It should be put at `~/aws-fpga`.
We are using shell version 1.4.5.
We have created a fork for the HDK repo, so we can directly clone from the fork:

        $ cd ~
        $ git clone https://github.com/csail-csg/aws-fpga.git
        $ cd aws-fpga
        $ git checkout riscy-OOO

- Make sure that Xilinx synthesis tool `vivado` is in PATH.
The vivado version we are using on AWS is `v2018.2_AR71275_op`.

- Clone this `riscy-OOO` repo, get all the submodules, and setup environment variables.

        $ cd <place you want to put this repo> # this should be a location on the shared file system
        $ git clone https://github.com/csail-csg/riscy-OOO.git
        $ cd riscy-OOO
        $ git submodule update --init --recursive
        $ source ./setup.sh

- If you would like to get email notification when the FPGA compilation finishes, you can do the following.

        $ export EMAIL=<email address>
        $ export SNS_NOTIFY_EMAIL=$EMAIL

### Compilation (and synthesis) of hardware on C4

- Compile the hardware part.
The following commands build the hardware for `$N` cores.
The logic resources on the FPGA are not unlimited, and we can at most fit 4 cores on the FPGA (see the [Other build configurations](#other-build-configurations) section), so we suggest to begin with `$N=1`.
We need to pass in the path for device tree compiler to the makefile to help later build of software part (C4 does not have device tree compiler).

        $ cd $RISCY_HOME/procs/RV64G_OOO
        $ make gen.awsf1 CORE_NUM=$N DTC_PATH=/usr/bin/dtc
        $ cd ../build/RV64G_OOO.core_$N.core_SMALL.cache_LARGE.weak.l1_cache_lru.check_deadlock/awsf1
        $ make bits -j16

    After the build finishes, we can find out the IDs of the FPGA image in `$RISCY_HOME/procs/build/RV64G_OOO.core_$N.core_SMALL.cache_LARGE.weak.l1_cache_lru.check_deadlock/awsf1/build/scripts/fpga_image_ids.json`, i.e., FPGA image ID `afi-xxx` and FPGA image global ID `agfi-yyy`.

- Wait for the FPGA image to be available.
We run the following command on C4 to monitor the state of the FPGA image.

        $ aws ec2 describe-fpga-images --fpga-image-ids afi-xxx

    When the `State` field in the command output changes from `pending` to `available`, the FPGA image will be available and we can switch to F1 to run the design.

### Setup F1
Most of the setups in the [Getting Started on a Local Ubuntu Machine](#getting-started-on-a-local-ubuntu-machine) section are not needed on F1.
Here are the steps to setup F1:

- Install the Bluespec compiler.

- Get dependencies for RISC-V toolchain and connectal.

        $ sudo apt-get install autoconf automake autotools-dev curl libmpc-dev libmpfr-dev libgmp-dev gawk build-essential bison flex texinfo gperf libtool patchutils bc zlib1g-dev device-tree-compiler pkg-config python-ply

- Install connectal.

        $ sudo apt-add-repository -y ppa:jamey-hicks/connectal
        $ sudo apt-get update
        $ sudo apt-get install connectal

- Setup shared file system with F1 (e.g., using EFS).

- Get the AWS HDK repo and install SDK.

        $ cd ~
        $ git clone https://github.com/csail-csg/aws-fpga.git
        $ cd aws-fpga
        $ git checkout riscy-OOO
        $ source sdk_setup.sh

- Build RISC-V front-end server.

        $ cd /path/to/riscy-OOO # go to the riscy-OOO repo on the shared file system
        $ cd tools
        $ ./build-fesvr.sh 8 # build using 8 threads
        $ cd ..
        
- Setup environment variables.

        $ source ./setup.sh

### Run on the FPGA of F1
- Finish compilation of software part.

        $ cd $RISCY_HOME/procs/build/RV64G_OOO.core_$N.core_SMALL.cache_LARGE.weak.l1_cache_lru.check_deadlock/awsf1
        $ make exe
 
- Program FPGA.
 
        $ sudo fpga-load-local-image -S 0 -I agfi-yyy

- Run the design to boot Linux.
We need to copy the bbl (e.g., `tools/RV64G/build-pk/bbl`) and boot rom (e.g., `procs/rom/out/rom_core_$N`) to F1.
The following command boots Linux with 2GB memory.

        $ $RISCY_HOME/procs/build/RV64G_OOO.core_$N.core_SMALL.cache_LARGE.weak.l1_cache_lru.check_deadlock/awsf1/bin/ubuntu.exe --core-num $N --mem-size 2048 --ignore-user-stucks 1000000 --rom /path/to/rom_core_$N --elf /path/to/bbl
    
    The processor detects potential deadlock by checking if a user level instruction has been executed during a period of time.
    This will output a lot of "deadlock" warnings when the processor is booting linux or idling in shell.
    To avoid such warnings, we use the `--ignore-user-stucks A_LARGE_NUMBER` option as shown above.
    This will suppress the first `A_LARGE_NUMBER` of user-level instruction deadlock messages.
    
    Hit `ctrl-c` when you want to exit.
    
    It should be noted that we need to program the FPGA before each run of the design (even if the design does not change).

### Other build configurations
`$RISCY_HOME/procs/RV64G_OOO/Makefile` contains several options to configure the build.
For example, the makefile can be invoked in the following way to build for C4:

    $ cd $RISCY_HOME/procs/RV64G_OOO
    $ make gen.awsf1 CORE_NUM=$N DTC_PATH=/usr/bin/dtc CORE_SIZE=<TINY/SMALL/MEDIUM/...> CACHE_SIZE=<LARGE/MC_2MB/...> TSO_MM=<true/false> STORE_PREFETCH=<true/false> CHECK_DEADLOCK=<true/false> USER_CLK_PERIOD=<clock period in ns>

Below are the expanations for these options.
It should be noted that these options can also be applied when building for simulation (i.e., for `make build.verilator`).

- `CORE_SIZE`: the size of each core in the processor.
The detailed buffer sizes for each `CORE_SIZE` configuration are defined in `$RISCY_HOME/procs/RV64G_OOO/ProcConfig.bsv`.
Default value is `SMALL` (64-entry ROB).

- `CACHE_SIZE`: the size of caches in the processor.
The detailed parameters for each `CACHE_SIZE` configuration are defined in `$RISCY_HOME/procs/RV64G_OOO/ProcConfig.bsv`.
Default value is `LARGE`.

- `TSO_MM`: enable TSO memory model or not.
If set to `true`, the processor implements TSO; otherwise, the processor implements a weak memroy model *WMM* (https://doi.org/10.1109/PACT.2017.29).
Default is `false`.

- `STORE_PREFETCH`: enable prefetch of exlusive cache permissions for store instructions.
This can be used to improve the performance of TSO (i.e., in case of `TSO_MM=true`).
Default value is `false`.

- `CHECK_DEADLOCK`: enable or disable the check on potential deadlock.
If set to `true`, the processor sends out a message to host software in case an instruction has been stuck at the ROB head for too long or a memory access has been stuck at cache MSHR for too long.
Otherwise, no such check will be performed.
Default is `true`.

- `USER_CLK_PERIOD`: the FPGA clock period for the processor in nano seconds.
The default value depends on the `CORE_SIZE` configuration.
It is recommended to make the clock period a multple of 8, because the AWS FPGA shell clock period is 8ns, and an async reset signal in our design is derived from the FPGA shell reset.
Doing so can prevent Xilinx Vivado from overconstraining the timing related to this async reset.

As an example, when we build the 4-core TSO multiprocessor on AWS, we invoke the makefile in the following way:

    $ cd $RISCY_HOME/procs/RV64G_OOO
    $ make gen.awsf1 CORE_NUM=4 DTC_PATH=/usr/bin/dtc CORE_SIZE=TINY CACHE_SIZE=MC_2MB TSO_MM=true STORE_PREFETCH=true USER_CLK_PERIOD=32

Since 4 OOO cores will make the FPGA pretty congested, we use the smallest core and cache configurations (`TINY` and `MC_2MB`, respectively).
We also increase the clock period to 32ns.

## Performance Counter
To collect performance data, we have deployed many performance counters in the processor design, and these counters can be queried by host software (see `$RISCY_HOME/procs/cpp/PerfStats.h`).
In addition, we added two custom user-level CSRs: the `stats` CSR (address `0x801`) and the `terminate` CSR (address `0x800`).
The `stats` CSR controls whether performance counters will be incremented, and the change made to the `stats` CSR by one core will be propagated to all other cores in a few cycles.
Any write to the `terminate` CSR done by any core will shutdown the processor, and send a message to the host software.
Then the host software will query all the performance counters.
This is a better way than using `ctrl-c` to exit Linux, because using `ctrl-c` will just kill everything and performance counters will not be dumped.

`$RISCY_HOME/riscv_custom/riscv_cumstom.h` contains C macros to set these two CSRs, and `$RISCY_HOME/riscv_custom/terminate` contains a simple C program to shutdown the processor using the `terminate` CSR.
This program is always included into the Linux image built using our script (see the [Getting Started on a Local Ubuntu Machine](#getting-started-on-a-local-ubuntu-machine) section).

## Known Issues

- When booting a BBL of a large size on a multicore implementation, the processor may output false deadlock warnings while the BBL is tranferred from the x86 host to the FPGA.
These warnings are because of the unfair arbitration between MMIO requests from different cores.
Core 0 keeps sending MMIO requests to check if the transfer of BBL is done, and this prevents other cores from using MMIO requests to fetch instructions from the boot rom.
These false deadlock warnings happens only during the transfer of BBL, and do not affect functionality or performance measurement.

## Directory Structure
Here we list some importand directories:

- `$RISCY_HOME/procs/lib`: contains BSV sources for processor building blocks.

- `$RISCY_HOME/procs/RV64G_OOO`: contains BSV sources for the top-level rules of the OOO processor.

- `$RISCY_HOME/procs/cpp`: contains the C++ sources for the host software that initalizes the RISC-V processor with the Linux image, and dumps performance data when the processor is shutdown.

- `$RISCY_HOME/coherence/src`: contains the BSV sources for the coherent caches.

- `$RISCY_HOME/fpgautils`: contains files to generate Xilinx FPGA IP blocks (e.g., floating-point units) and BSV wrappers.

- `$RISCY_HOME/connectal`: contains the Connectal repo, which is the framework we are using for software-FPGA communication.

- `$RISCY_HOME/tools`: contains the RISC-V toolchain, the Linux kernel, and some prebuilt Linux images.

**Obsolete directories**: `$RISCY_HOME/procs/RV64G_MultiCycle` and `$RISCY_HOME/procs/RV64G_InOrder` contain multi-cycle and in-order processors derived from the OOO processor, respectively.
However, both processors are no longer maintained, so please ignore these two folders.

## VC707 FPGA
**We have not tested VC707 FPGA for a while, so it is recommonded to use AWS FPGAs.**

It is also possible to run the design on a VC707 FPGA, but the VC707 FPGA can only hold 1 core.
We connect a VC707 FPGA to a Ubuntu machine through PCIe.
The Ubuntu machine should be setup following the steps in the [Getting Started on a Local Ubuntu Machine](#getting-started-on-a-local-ubuntu-machine) section.

The communication through the PCIe link may not be very stable, because the version of Connectal (`$RISCY_HOME/connectal`) in this repo is not the latest.
These issues have been fixed in the latest version of Connectal, but we have not got a chance to upgrade to the latest version.
Therefore, we suggest to use AWS if possible.

- Build for VC707 FPGA.
We are using Xilinx Vivado 2015.4 on Ubuntu 14.04 or Ubuntu 16.04.
(Higher Vivado versions may fail.)
Also make sure that `vivado` is in PATH.
VC707 shoud only be able to hold 1 core.

        $ cd $RISCY_HOME/procs/RV64G_OOO
        $ make build.vc707 CORE_NUM=1 USER_CLK_PERIOD=40 # use a slower clock

    The build result will be in `$RISCY_HOME/procs/build/RV64G_OOO.core_1.core_SMALL.cache_LARGE.weak.l1_cache_lru.deadlock_check/vc707/bin`.
    The other build options can be passed to the makefile as in AWS.
    
- Boot Linux on FPGA.
Since VC707 only has 1GB DRAM, we boot Linux with 1GB memory.

        $ $RISCY_HOME/procs/build/RV64G_OOO.core_1.core_SMALL.cache_LARGE.weak.l1_cache_lru.check_deadlock/vc707/bin/ubuntu.exe --core-num 1 --mem-size 1024  --ignore-user-stucks 1000000 --rom /path/to/rom_core_$N --elf /path/to/bbl

    The above command will automaticall program the FPGA.
    If it is the first time to program the VC707 FPGA, the program may fail to run.
    Just reboot the machine and re-run the above command.

## Publications

- Sizhuo Zhang, Andrew Wright, Thomas Bourgeat, Arvind. "Composable Building Blocks to Open up Processor Design." In IEEE Micro, Volume 39, Issue 3, May-June 2019.

- Sizhuo Zhang, Andrew Wright, Thomas Bourgeat, Arvind. "Composable Building Blocks to Open up Processor Design." In the 51st IEEE/ACM International Symposium on Microarchitecture (MICRO), October, 2018.
