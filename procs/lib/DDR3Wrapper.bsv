import DDR3Common::*;
import DDR3Controller::*;
import DefaultValue::*;

typedef 128 DDR3MaxReadNum;
typedef 10 DDR3SimDelay;

typedef DDR3_1GB_User#(DDR3MaxReadNum, DDR3SimDelay) DDR3User;
typedef DDR3_1GB_Controller#(DDR3MaxReadNum, DDR3SimDelay) DDR3Wrapper;

(* synthesize *)
module mkDDR3Wrapper#(Clock sys_clk, Reset sys_rst)(DDR3Wrapper);
    let m <- mkDDR3_1GB_Controller(sys_clk, sys_rst, defaultValue);
    return m;
endmodule
