import Types::*;
import TraceBuffer::*;
import StmtFSM::*;
import GetPut::*;
import ConfigReg::*;

typedef 128 PostTriggerCnt;

(* synthesize *)
module mkTb(Empty);
    Reg#(Data) clk <- mkConfigReg(0);
    TraceBuffer#(2, Data) trace <- mkTraceBuffer(regToReadOnly(clk), valueof(PostTriggerCnt));

    Reg#(File) enqLog <- mkReg(InvalidFile);
    Reg#(File) deqLog <- mkReg(InvalidFile);

    Reg#(Data) cnt <- mkReg(0); // number of cycles of enque
    Reg#(Data) data <- mkReg(0); // data to enq
    Reg#(Data) toDeq <- mkReg(0); // expected trace buffer output
    Reg#(Data) firstDeq <- mkReg(0); // first data that is deq

    Stmt test = (seq
        // init
        action
            let f <- $fopen("enq.log", "w");
            enqLog <= f;
            f <- $fopen("deq.log", "w");
            deqLog <= f;
        endaction

        // enq trace
        while(cnt < fromInteger(2 * valueof(TraceSize)))
        action
            case(clk[1:0])
                2'b01: begin
                    trace.enq[0].put(data);
                    data <= data + 1;
                    cnt <= cnt + 1;
                    $fdisplay(enqLog, "clk %d, data %d", clk, data);
                end
                2'b10: begin
                    trace.enq[1].put(data);
                    data <= data + 1;
                    cnt <= cnt + 1;
                    $fdisplay(enqLog, "clk %d, data %d", clk, data);
                end
                2'b11: begin
                    trace.enq[0].put(data);
                    trace.enq[1].put(data + 1);
                    data <= data + 2;
                    cnt <= cnt + 1;
                    $fdisplay(enqLog, "clk %d, data %d", clk, data);
                    $fdisplay(enqLog, "clk %d, data %d", clk, data + 1);
                end
            endcase
            clk <= clk + 1;
        endaction

        $display("Total enq %d items", cnt);

        // trigger
        trace.trigger;

        // post trigger trace
        while(cnt < fromInteger(valueof(TraceSize)))
        action
            case(clk[1:0])
                2'b01: begin
                    trace.enq[0].put(data);
                    data <= data + 1;
                    cnt <= cnt + 1;
                    $fdisplay(enqLog, "clk %d, data %d", clk, data);
                end
                2'b10: begin
                    trace.enq[1].put(data);
                    data <= data + 1;
                    cnt <= cnt + 1;
                    $fdisplay(enqLog, "clk %d, data %d", clk, data);
                end
                2'b11: begin
                    trace.enq[0].put(data);
                    trace.enq[1].put(data + 1);
                    data <= data + 2;
                    cnt <= cnt + 1;
                    $fdisplay(enqLog, "clk %d, data %d", clk, data);
                    $fdisplay(enqLog, "clk %d, data %d", clk, data + 1);
                end
            endcase
            clk <= clk + 1;
        endaction

        // deq trace
        action
            let {c, d} <- trace.deq.get;
            firstDeq <= d;
            toDeq <= d + 1;
            $display("First deq clk %d, data %d", c, d);
            $fdisplay(deqLog, "clk %d, data %d", c, d);
        endaction
        while(toDeq < firstDeq + fromInteger(valueof(TraceSize)))
        action
            let {c, d} <- trace.deq.get;
            doAssert(d == toDeq, "must match");
            toDeq <= toDeq + 1;
            $fdisplay(deqLog, "clk %d, data %d", c, d);
        endaction

        $display("PASS");
    endseq);
    mkAutoFSM(test);
endmodule
