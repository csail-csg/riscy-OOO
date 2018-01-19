
# portal clk
set portal_clk [get_clocks -of_objects [get_pins host_pcieHostTop_ep7/clkgen_pll/CLKOUT1]]
set portal_period [get_property PERIOD [get_clocks $portal_clk]]
# user clk
set user_clk [get_clocks -of_objects [get_pins -hier -filter {NAME =~ *userClkGenerator_pll/CLKOUT0}]]
set user_period [get_property PERIOD [get_clocks $user_clk]]

# This should only affect path from portal reset to user reset, timings for
# other cross-clock paths should be overriden by sync FIFO IP core
set_max_delay -from [get_clocks $portal_clk] -to [get_clocks $user_clk] -datapath_only [expr min($portal_period, $user_period)]
