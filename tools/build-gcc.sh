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

. build.common

# Build riscv-gnu-toolchain
OUTPUT_FILE=$OUTPUT_PATH/riscv-gnu-toolchain.log
echo "Building riscv-gnu-toolchain... (writing output to $OUTPUT_FILE)"
cd $RISCV
mkdir -p build-gnu-toolchain
cd build-gnu-toolchain
../../riscv-gnu-toolchain/configure --prefix=$RISCV --with-arch=rv64imafd &> $OUTPUT_FILE
make -j$JOBS &>> $OUTPUT_FILE
make linux -j$JOBS &>> $OUTPUT_FILE

cd $STARTINGDIR

echo ""
echo "RV64G riscv-gnu-toolchain compiled successfully."
