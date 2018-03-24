if { $argc != 2 } {
    puts "Usage: top.tcl DESIGN_VERIFLOG_DIR OUT_DIR"
    exit
}

set designDir [lindex $argv 0]
set outDir [lindex $argv 1]

set partName {xcvu9p-flgb2104-2-i}

# create out dir
file mkdir $outDir

# read in source
read_verilog [ glob $designDir/*.v ]
read_verilog [ glob ../bluespec_verilog/*.v ]
read_xdc ./top.xdc

# synth
synth_design -top mkCoreTop -part $partName -flatten rebuilt
write_checkpoint -force $outDir/post_synth.dcp
report_timing_summary -file $outDir/post_synth_timing_summary.rpt
report_utilization -file $outDir/post_synth_util.rpt

