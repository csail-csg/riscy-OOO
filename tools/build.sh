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

if [ $# -ne 0 ]; then
    JOBS=$1
else
    JOBS=1
fi

echo "Building RV64G toolchain..."

STARTINGDIR=$PWD
export RISCV=$PWD/RV64G
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
../../riscv-gnu-toolchain/configure --prefix=$RISCV --with-arch=rv64imafd &> $OUTPUT_FILE
make -j$JOBS &>> $OUTPUT_FILE
make linux -j$JOBS &>> $OUTPUT_FILE

# Build riscv-fesvr
OUTPUT_FILE=$OUTPUT_PATH/riscv-fesvr.log
echo "Building riscv-fesvr... (writing output to $OUTPUT_FILE)"
cd $RISCV
mkdir -p build-fesvr
cd build-fesvr
../../riscv-fesvr/configure --prefix=$RISCV &> $OUTPUT_FILE
make -j$JOBS &>> $OUTPUT_FILE
make install &>> $OUTPUT_FILE

# Build riscv-tests
OUTPUT_FILE=$OUTPUT_PATH/riscv-tests.log
echo "Building riscv-tests... (writing output to $OUTPUT_FILE)"
cd $RISCV
mkdir -p build-tests
cd build-tests
../../riscv-tests/configure --prefix=$RISCV/riscv64-unknown-elf &> $OUTPUT_FILE
make -j$JOBS &>> $OUTPUT_FILE
make install &>> $OUTPUT_FILE

# Build riscv-isa-sim
OUTPUT_FILE=$OUTPUT_PATH/riscv-isa-sim.log
echo "Building riscv-isa-sim... (writing output to $OUTPUT_FILE)"
cd $RISCV
mkdir -p build-isa-sim
cd build-isa-sim
../../riscv-isa-sim/configure --prefix=$RISCV --with-fesvr=$RISCV --with-isa=RV64IMAFD &> $OUTPUT_FILE
make -j$JOBS &>> $OUTPUT_FILE
make install &>> $OUTPUT_FILE

# Build riscv-pk
OUTPUT_FILE=$OUTPUT_PATH/riscv-pk.log
echo "Building riscv-pk... (writing output to $OUTPUT_FILE)"
cd $RISCV
mkdir -p build-pk
cd build-pk
../../riscv-pk/configure --prefix=$RISCV --host=riscv64-unknown-elf &> $OUTPUT_FILE
make -j$JOBS &>> $OUTPUT_FILE
make install &>> $OUTPUT_FILE

cd $STARTINGDIR

echo ""
echo "RV64G toolchain and riscv-tests compiled successfully."
