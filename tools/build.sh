#!/bin/bash

# Copyright (c) 2017 Massachusetts Institute of Technology

# Permission is hereby granted, free of charge, to any person
# obtaining a copy of this software and associated documentation
# files (the "Software"), to deal in the Software without
# restriction, including without limitation the rights to use, copy,
# modify, merge, publish, distribute, sublicense, and/or sell copies
# of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:

# The above copyright notice and this permission notice shall be
# included in all copies or substantial portions of the Software.

# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
# EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
# MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
# NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
# BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
# ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
# CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

# Abort on error
set -e

# do RV64G by default
XLEN=64

# input arg may say do soft-fp
SOFTFP="no"
if [ $# -eq 1 ] ; then
    if [ $1 = "softfp" ]; then
        SOFTFP="yes"
    else
        echo "Usage: $0 [softfp]"
        return 1
    fi
fi

if [ $SOFTFP = "yes" ]; then
    ISA="RV${XLEN}IMA"
    WITH_ARCH=" --with-arch=IMA --with-xlen=$XLEN "
else
    ISA="RV${XLEN}G"
    WITH_ARCH=" --with-arch=IMAFD --with-xlen=$XLEN "
fi

echo "Building $ISA toolchain..."

STARTINGDIR=$PWD
export RISCV=$PWD/$ISA
export PATH=$RISCV/bin:$PATH
OUTPUT_PATH=$RISCV/build-log

mkdir -p $RISCV
mkdir -p $OUTPUT_PATH

# Build riscv-gnu-toolchain
OUTPUT_FILE=$OUTPUT_PATH/riscv-gnu-toolchain.log
echo "Building riscv-gnu-toolchain... (writing output to $OUTPUT_FILE)"
cd $RISCV
mkdir -p build-gnu-toolchain
cd build-gnu-toolchain
../../riscv-gnu-toolchain/configure --prefix=$RISCV $WITH_ARCH &> $OUTPUT_FILE
make &>> $OUTPUT_FILE
# gnu-linux-gcc won't build with soft fp
if [ $SOFTFP = "no" ]; then
    make linux &>> $OUTPUT_FILE
fi

# [sizhuo] XXX why rebuild??
## Rebuild newlib with -mcmodel=medany
#OUTPUT_FILE=$OUTPUT_PATH/newlib.log
#echo "Rebuilding newlib... (writing output to $OUTPUT_FILE)"
#cd build-gcc-newlib/riscv$XLEN-unknown-elf/newlib
#sed 's/^CFLAGS = /CFLAGS = -mcmodel=medany /' Makefile > Makefile.sed
#mv Makefile.sed Makefile
#make clean &> $OUTPUT_FILE
#make &>> $OUTPUT_FILE
#make install &>> $OUTPUT_FILE

# Build riscv-tests
OUTPUT_FILE=$OUTPUT_PATH/riscv-tests.log
echo "Building riscv-tests... (writing output to $OUTPUT_FILE)"
cd $RISCV
mkdir build-tests
cd build-tests
../../riscv-tests/configure --prefix=$RISCV/riscv$XLEN-unknown-elf &> $OUTPUT_FILE
# This may fail since some riscv-tests require ISA extensions
# Also there is an issue with building 32-bit executables when gcc is
# configured with --with-arch=<isa>
make &>> $OUTPUT_FILE
make install &>> $OUTPUT_FILE

# Build riscv-fesvr
OUTPUT_FILE=$OUTPUT_PATH/riscv-fesvr.log
echo "Building riscv-fesvr... (writing output to $OUTPUT_FILE)"
cd $RISCV
mkdir -p build-fesvr
cd build-fesvr
../../riscv-fesvr/configure --prefix=$RISCV &> $OUTPUT_FILE
make &>> $OUTPUT_FILE
make install &>> $OUTPUT_FILE

# Build riscv-isa-sim
OUTPUT_FILE=$OUTPUT_PATH/riscv-isa-sim.log
echo "Building riscv-isa-sim... (writing output to $OUTPUT_FILE)"
cd $RISCV
mkdir -p build-isa-sim
cd build-isa-sim
../../riscv-isa-sim/configure --prefix=$RISCV --with-fesvr=$RISCV &> $OUTPUT_FILE
make &>> $OUTPUT_FILE
make install &>> $OUTPUT_FILE

# Build riscv-pk
OUTPUT_FILE=$OUTPUT_PATH/riscv-pk.log
echo "Building riscv-pk... (writing output to $OUTPUT_FILE)"
cd $RISCV
mkdir -p build-pk
cd build-pk
../../riscv-pk/configure --prefix=$RISCV --host=riscv${XLEN}-unknown-elf &> $OUTPUT_FILE
make &>> $OUTPUT_FILE
make install &>> $OUTPUT_FILE

cd $STARTINGDIR

echo ""
echo "$ISA toolchain and riscv-tests compiled successfully."
