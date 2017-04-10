# opt_design cmd by default will do power optimization on BRAM
# our design may get error during this optimization, so let it optimize nothing.
# TODO: it will be great if we could pass arguments to opt_design cmd
# to avoid wasting time on power optimization
set_power_opt -cell_types {none}
