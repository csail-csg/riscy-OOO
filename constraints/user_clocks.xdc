set delay_ratio 1

# portal clk
set portal_clk [get_clocks -of_objects [get_pins host_pcieHostTop_ep7/clkgen_pll/CLKOUT1]]
# user clk
set user_clk [get_clocks -of_objects [get_pins -hier -filter {NAME =~ *userClkGenerator_pll/CLKOUT0}]]
# ddr3 app clk
set ddr3_app_clk [get_clocks -of_objects [get_pins -hier -filter {NAME =~ */u_ddr3_infrastructure/gen_mmcm.mmcm_i/CLKFBOUT}]]

# user clk <-> portal clk
set_max_delay -from [get_clocks $user_clk] -to [get_clocks $portal_clk] -datapath_only [expr $delay_ratio * [get_property PERIOD [get_clocks $portal_clk]]]
set_max_delay -from [get_clocks $portal_clk] -to [get_clocks $user_clk] -datapath_only [expr $delay_ratio * [get_property PERIOD [get_clocks $user_clk]]]

# user clk <-> ddr3 app clk
set_max_delay -from [get_clocks $user_clk] -to [get_clocks $ddr3_app_clk] -datapath_only [expr $delay_ratio * [get_property PERIOD [get_clocks $ddr3_app_clk]]]
set_max_delay -from [get_clocks $ddr3_app_clk] -to [get_clocks $user_clk] -datapath_only [expr $delay_ratio * [get_property PERIOD [get_clocks $user_clk]]]

# ddr3 app clk <-> portal clk: init_calib_done signal from ddr3 app is passed to indication
# this should be async
set_clock_groups -asynchronous -group $ddr3_app_clk -group $portal_clk

