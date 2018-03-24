#!/bin/bash

if [ $# -ne 2 ]; then
    echo "Usage: ./compile DESIGN_VERILOG_DIR OUT_DIR"
    exit
fi

vivado -mode batch -source top.tcl -tclargs $1 $2
