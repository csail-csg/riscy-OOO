`include "ProcConfig.bsv"

import BrPred::*;
import DirPredictor::*;
import Btb::*;
import ClientServer::*;
import Connectable::*;
import Decode::*;
import Ehr::*;
import Fifo::*;
import GetPut::*;
import MemoryTypes::*;
import Types::*;
import ProcTypes::*;
import CCTypes::*;
import Ras::*;
import EpochManager::*;
import Performance::*;
import Vector::*;
import Assert::*;
import Cntrs::*;
import ConfigReg::*;
import TlbTypes::*;
import ITlb::*;
import CCTypes::*;
import L1CoCache::*;

interface FetchStage;
    // pipeline
    interface Vector#(SupSize, SupFifoDeq#(FromFetchStage)) pipelines;

    // tlb and mem connections
    interface ITlb iTlbIfc;
    interface ICoCache iMemIfc;

    // starting and stopping
    method Action start(Addr pc);
    method Action stop();

    // redirection methods
    method Action setWaitRedirect;
    method Action redirect(Addr pc);
    method Action done_flushing();
    method Action train_predictors(
        Addr pc, Addr next_pc, IType iType, Bool taken,
        DirPredTrainInfo dpTrain, Bool mispred
    );

    // debug
    method FetchDebugState getFetchState;

    // performance
    interface Perf#(DecStagePerfType) perf;
endinterface

typedef struct {
    Addr pc;
    Epoch mainEp;
    Bool waitForRedirect;
    Bool waitForFlush;
} FetchDebugState deriving(Bits, Eq, FShow);

typedef struct {
    Addr pc;
    Addr pred_next_pc;
    Bool decode_epoch;
    Epoch main_epoch;
} Fetch1ToFetch2 deriving(Bits, Eq, FShow);

typedef struct {
    Addr pc;
    Addr phys_pc;
    Addr pred_next_pc;
    Maybe#(Exception) cause;
    Bool decode_epoch;
    Epoch main_epoch;
} Fetch2ToFetch3 deriving(Bits, Eq, FShow);

typedef struct {
  Addr pc;
  Addr ppc;
  Bool decode_epoch;
  Epoch main_epoch;
  Instruction inst;
  Maybe#(Exception) cause;
} Fetch3ToDecode deriving(Bits, Eq, FShow);

typedef struct {
  Addr pc;
  Addr ppc;
  Epoch main_epoch;
  DirPredTrainInfo dpTrain;
  Instruction inst;
  DecodedInst dInst;
  ArchRegs regs;
  Maybe#(Exception) cause;
} FromFetchStage deriving (Bits, Eq, FShow);

// train next addr pred (BTB)
typedef struct {
    Addr pc;
    Addr nextPc;
} TrainNAP deriving(Bits, Eq, FShow);

(* synthesize *)
module mkFetchStage(FetchStage);
    // rule ordering: Fetch1 (BTB+TLB) < Fetch3 (decode & dir pred) < redirect method
    // Fetch1 < Fetch3 to avoid bypassing path on PC and epochs

    let verbose = True;

    // Basic State Elements
    Reg#(Bool) started <- mkReg(False);

    // Stall fetch when trap happens or system inst is renamed
    // All inst younger than the trap/system inst will be killed
    // Since CSR may be modified, sending wrong path request to TLB may cause problem
    // So we stall until the next redirection happens
    // The next redirect is either by the trap/system inst or an older one
    Reg#(Bool) waitForRedirect <- mkReg(False);
    // We don't want setWaitForRedirect method and redirect method to happen together
    // make them conflict
    RWire#(void) setWaitRedirect_redirect_conflict <- mkRWire;

    // Stall fetch during the flush triggered by the procesing trap/system inst in commit stage
    // We stall until the flush is done
    Reg#(Bool) waitForFlush <- mkReg(False);

    Ehr#(3, Addr) pc_reg <- mkEhr(0);
    Integer pc_fetch1_port = 0;
    Integer pc_decode_port = 1;
    Integer pc_redirect_port = 2;

    // Epochs
    Reg#(Bool) decode_epoch <- mkReg(False);
    Reg#(Epoch) f_main_epoch <- mkReg(0); // fetch estimate of main epoch

    // Pipeline Stage FIFOs
    Fifo#(2, Tuple2#(Bit#(TLog#(SupSize)),Fetch1ToFetch2)) f12f2 <- mkCFFifo;
    Fifo#(4, Tuple2#(Bit#(TLog#(SupSize)),Fetch2ToFetch3)) f22f3 <- mkCFFifo; // FIFO should match I$ latency
    SupFifo#(SupSize, 2, FromFetchStage) out_fifo <- mkSupFifo;

    // Branch Predictors
    NextAddrPred    nextAddrPred <- mkBtb;
    let             dirPred      <- mkDirPredictor;
    ReturnAddrStack ras          <- mkRas;
    // Wire to train next addr pred (NAP)
    RWire#(TrainNAP) napTrainByDec <- mkRWire;
    RWire#(TrainNAP) napTrainByExe <- mkRWire;

    // TLB and Cache connections
    ITlb iTlb <- mkITlb;
    ICoCache iMem <- mkICoCache;
    Server#(Addr, TlbResp) tlb_server = iTlb.to_proc;
    Server#(Addr, Vector#(SupSize, Maybe#(Instruction))) mem_server = iMem.to_proc;

    // performance counters
    Fifo#(1, DecStagePerfType) perfReqQ <- mkCFFifo; // perf req FIFO
`ifdef PERF_COUNT
    Reg#(Bool) doStats <- mkConfigReg(False);
    // decode stage redirect
    Count#(Data) decRedirectBrCnt <- mkCount(0);
    Count#(Data) decRedirectJmpCnt <- mkCount(0);
    Count#(Data) decRedirectJrCnt <- mkCount(0);
    Count#(Data) decRedirectOtherCnt <- mkCount(0);
    // perf resp FIFO
    Fifo#(1, PerfResp#(DecStagePerfType)) perfRespQ <- mkCFFifo;

    rule doPerfReq;
        let t <- toGet(perfReqQ).get;
        Data d = (case(t)
            DecRedirectBr: decRedirectBrCnt;
            DecRedirectJmp: decRedirectJmpCnt;
            DecRedirectJr: decRedirectJrCnt;
            DecRedirectOther: decRedirectOtherCnt;
            default: 0;
        endcase);
        perfRespQ.enq(PerfResp {
            pType: t,
            data: d
        });
    endrule
`endif

    //rule debugEmptyness;
    //    if(verbose && !f12f2.notFull) $display("First fifo f12f2 full");
    //endrule
    //rule debugEmptyness2;
    //    if(!f22f3.notEmpty) begin
    //        //$fdisplay(stdout, "out_fifo between decode and renaming is empty");
    //    end
    //endrule
    //rule debugEMptyness3;
    //    if (verbose && !out_fifo.notEmpty) $display("superscalar out fifo full");
    //endrule

    // We don't send req to TLB when waiting for redirect or TLB flush
    // Since there is no FIFO between doFetch1 and TLB,
    // when OOO commit stage wait TLB idle and change VM CSR / signal flush TLB,
    // There is no wrong path request afterwards to race with the system code that manage paget table
    rule doFetch1(started && !waitForRedirect && !waitForFlush);
        let pc = pc_reg[pc_fetch1_port];

        // Chain of prediction for the next instructions
        // We need a BTB with a register file with enough ports!
        Vector#(SupSize, Addr) pred_future_pc = newVector;
        pred_future_pc[0] = nextAddrPred.predPc(pc);
        for (Integer i = 1; i < valueof(SupSize); i = i+1) begin
            pred_future_pc[i] = nextAddrPred.predPc(pred_future_pc[i-1]);
        end
        let pred_next_pc = ?;
        Integer posLastSup = ?;
        // Next pc is the first nextPc that breaks the chain of pc+4 or
        // that is at the end of a cacheline.
        Vector#(SupSize,Integer) indexes = genVector;
        function Bool findNextPc(Addr pc, Integer i);
            Bool notLastInst = getLineInstOffset(pc + fromInteger(4*i)) != maxBound;
            Bool noJump = pred_future_pc[i] == pc + fromInteger(4*(i+1));
            return (!(notLastInst && noJump));
            //return (!((pc+fromInteger(4*i))[5:2]!= 4'b1111 && pred_future_pc[i] == pc + fromInteger(4*(i+1))));
        endfunction

        posLastSup = fromMaybe(fromInteger(valueof(SupSize) - 1), find(findNextPc(pc), indexes));
        pred_next_pc = pred_future_pc[posLastSup];
        pc_reg[pc_fetch1_port] <= pred_next_pc;
        // Send TLB request
        tlb_server.request.put(pc);

        let out = Fetch1ToFetch2 {
            pc: pc,
            pred_next_pc: pred_next_pc,
            decode_epoch: decode_epoch,
            main_epoch: f_main_epoch};
        f12f2.enq(tuple2(fromInteger(posLastSup),out));
        if (verbose) $display("Fetch1: ", fshow(out));
    endrule

    rule doFetch2;
        let {nbSup,in} = f12f2.first;
        f12f2.deq;

        // Get TLB response
        match {.phys_pc, .cause} <- tlb_server.response.get;
        if (!isValid(cause)) begin
            $display("Send request to memory");
            // Send ICache request if no exception
            mem_server.request.put(phys_pc);
        end

        let out = Fetch2ToFetch3 {
            pc: in.pc,
            phys_pc: phys_pc,
            pred_next_pc: in.pred_next_pc,
            cause: cause,
            decode_epoch: in.decode_epoch,
            main_epoch: in.main_epoch };
        f22f3.enq(tuple2(nbSup,out));
        if (verbose) $display("Fetch2: ", fshow(out));
    endrule

    rule doFetch3;
        let {nbSup, fetch3In} = f22f3.first;
        f22f3.deq;
        if (verbose) $display("Fetch3 %d",fetch3In.pc);
        Instruction inst = 0;

        // Get ICache response if no exception
        Vector#(SupSize,Maybe#(Instruction)) inst_data = replicate(tagged Valid unpack(0));
        if(!isValid(fetch3In.cause)) begin
            if(verbose) $display("get answer from memory %d", fetch3In.pc);
            inst_data <- mem_server.response.get;
        end
        if(verbose) $display("epoch instr: %d, epoch main : %d", fetch3In.main_epoch, f_main_epoch);
        
        // The main_epoch check is required to make sure this stage doesn't
        // redirect the PC if a later stage already redirected the PC.
        if (fetch3In.main_epoch == f_main_epoch) begin
            Bool decode_epoch_local = decode_epoch; // next value for decode epoch
            Maybe#(Addr) redirectPc = Invalid; // next pc redirect by branch predictor
            Maybe#(TrainNAP) trainNAP = Invalid; // training data sent to next addr pred
`ifdef PERF_COUNT
            // performance counter: inst being redirect by decode stage
            // Note that only 1 redirection may happen in a cycle
            Maybe#(IType) redirectInst = Invalid;
`endif

            for (Integer i = 0; i < valueof(SupSize); i=i+1) begin
                if (inst_data[i] != tagged Invalid && fromInteger(i) <= nbSup) begin
                    // get the input to decode
                    Fetch3ToDecode decIn = ?;
                    if (fromInteger(i) == (nbSup)) begin
                        // Get ICache response if no exception
                        inst = fromMaybe(?,inst_data[i]);
                        decIn = Fetch3ToDecode {pc: fetch3In.pc+fromInteger(4*i),
                                              ppc: fetch3In.pred_next_pc,
                                              decode_epoch: fetch3In.decode_epoch,
                                              main_epoch: fetch3In.main_epoch,
                                              inst: inst,
                                              cause: fetch3In.cause};
                    end
                    else begin
                        inst = fromMaybe(?,inst_data[i]);
                        decIn = Fetch3ToDecode {pc: fetch3In.pc+fromInteger(4*i),
                                              ppc: fetch3In.pc+fromInteger(4*(i+1)),
                                              decode_epoch: fetch3In.decode_epoch,
                                              main_epoch: fetch3In.main_epoch,
                                              inst: inst,
                                              cause: fetch3In.cause};
                    end
                    let in = decIn;
                    let cause = in.cause;
                    $display("%d\n",i);

                    // do decode and branch prediction
                    // Drop here if does not match the decode_epoch.
                    if (in.decode_epoch == decode_epoch_local) begin
                        doAssert(in.main_epoch == f_main_epoch, "main epoch must match");

                        let decode_result = decode(in.inst);

                        if (!isValid(cause)) begin
                          //  update cause if there was not an early detected exception
                            cause = decode_result.illegalInst ? tagged Valid IllegalInst : tagged Invalid;
                        end

                        let dInst = decode_result.dInst;
                        let regs = decode_result.regs;
                        DirPredTrainInfo dp_train = ?; // dir pred training bookkeeping

                        // update predicted next pc
                        if (!isValid(cause)) begin
                            // direction predict
                            Bool pred_taken = False;
                            if(dInst.iType == Br) begin
                                let pred_res <- dirPred.pred[i].pred(in.pc);
                                pred_taken = pred_res.taken;
                                dp_train = pred_res.train;
                            end
                            Maybe#(Addr) nextPc = decodeBrPred(in.pc, dInst, pred_taken);

                            // return address stack
                            if (dInst.iType == J && isValid(regs.dst)) begin
                                // function call -- push return address to stack
                                ras.pushAddress[i](in.pc + 4);
                            end
                            else if (dInst.iType == Jr) begin
                                let new_ppc = ras.firstAddress[i];
                                ras.popAddress[i];
                                nextPc = tagged Valid new_ppc;
                            end

                            if(verbose) begin
                                $display("Branch prediction: ", fshow(dInst.iType), " ; ", fshow(in.pc), " ; ",
                                         fshow(in.ppc), " ; ", fshow(pred_taken), " ; ", fshow(nextPc));
                            end

                            // check previous mispred
                            if (nextPc matches tagged Valid .decode_pred_next_pc &&& decode_pred_next_pc != in.ppc) begin
                                if (verbose) $display("ppc and decodeppc :  %h %h", in.ppc, decode_pred_next_pc);
                                decode_epoch_local = !decode_epoch_local;
                                redirectPc = Valid (decode_pred_next_pc); // record redirect next pc
                                in.ppc = decode_pred_next_pc;
                                // train next addr pred when mispredict
                                trainNAP = Valid (TrainNAP {pc: in.pc, nextPc: decode_pred_next_pc});
`ifdef PERF_COUNT
                                // performance stats: record decode redirect
                                doAssert(redirectInst == Invalid, "at most 1 decode redirect per cycle");
                                redirectInst = Valid (dInst.iType);
`endif
                            end
                        end
                        let out = FromFetchStage{pc: in.pc,
                                                 ppc: in.ppc,
                                                 main_epoch: in.main_epoch,
                                                 dpTrain: dp_train,
                                                 inst: in.inst,
                                                 dInst: dInst,
                                                 regs: decode_result.regs,
                                                 cause: cause };
                        out_fifo.enqS[i].enq(out);
                        if (verbose) $display("Decode: ", fshow(out));
                    end
                    else begin
                        $display("Drop decoded within a superscalar");
                        // just drop wrong path instructions
                    end
                end
            end

            // update PC and epoch
            if(redirectPc matches tagged Valid .nextPc) begin
                pc_reg[pc_decode_port] <= nextPc;
            end
            decode_epoch <= decode_epoch_local;
            // send training data for next addr pred
            if(trainNAP matches tagged Valid .x) begin
                napTrainByDec.wset(x);
            end
`ifdef PERF_COUNT
            // performance counter: check whether redirect happens
            if(redirectInst matches tagged Valid .iType &&& doStats) begin
                case(iType)
                    Br: decRedirectBrCnt.incr(1);
                    J : decRedirectJmpCnt.incr(1);
                    Jr: decRedirectJrCnt.incr(1);
                    default: decRedirectOtherCnt.incr(1);
                endcase
            end
`endif
        end
        else begin
            $display("drop in fetch3decode");
        end
    endrule

    // train next addr pred
    (* fire_when_enabled, no_implicit_conditions *)
    rule doTrainNAP(isValid(napTrainByDec.wget) || isValid(napTrainByExe.wget));
        // give priority to train data from exe
        TrainNAP train = fromMaybe(validValue(napTrainByDec.wget), napTrainByExe.wget);
        nextAddrPred.update(train.pc, train.nextPc, train.nextPc != train.pc + 4);
    endrule

    interface Vector pipelines = out_fifo.deqS;
    interface iTlbIfc = iTlb;
    interface iMemIfc = iMem;

    method Action start(Addr start_pc);
        pc_reg[0] <= start_pc;
        started <= True;
        waitForRedirect <= False;
        waitForFlush <= False;
    endmethod
    method Action stop();
        started <= False;
    endmethod

    method Action setWaitRedirect;
        waitForRedirect <= True;
        setWaitRedirect_redirect_conflict.wset(?); // conflict with redirect
    endmethod
    method Action redirect(Addr new_pc);
        if (verbose) $display("Redirect: newpc %h, old f_main_epoch %d, new f_main_epoch %d",new_pc,f_main_epoch,f_main_epoch+1);
        pc_reg[pc_redirect_port] <= new_pc;
        f_main_epoch <= (f_main_epoch == fromInteger(valueOf(NumEpochs)-1)) ? 0 : f_main_epoch + 1;
        // redirect comes, stop stalling for redirect
        waitForRedirect <= False;
        setWaitRedirect_redirect_conflict.wset(?); // conflict with setWaitForRedirect
        // this redirect may be caused by a trap/system inst in commit stage
        // we conservatively set wait for flush TODO make this an input parameter
        waitForFlush <= True;
    endmethod
    method Action done_flushing() if (waitForFlush);
        // signal that the pipeline can resume fetching
        waitForFlush <= False;
        // XXX The guard prevents the readyToFetch rule in Core.bsv from firing every cycle
        // The guard also makes this method sequence before (restricted) redirect method
        // So the effect of setting waitForFlush in redirect method will not be overwritten
        // Then we don't need to make two methods conflict
        // It's fine for the effect of this method to be overwritten, because it fires very often
    endmethod

    method Action train_predictors(
        Addr pc, Addr next_pc, IType iType, Bool taken,
        DirPredTrainInfo dpTrain, Bool mispred
    );
        //if (iType == J || (iType == Br && next_pc < pc)) begin
        //    // Only train the next address predictor for jumps and backward branches
        //    // next_pc != pc + 4 is a substitute for taken
        //    nextAddrPred.update(pc, next_pc, taken);
        //end
        if (iType == Br) begin
            // Train the direction predictor for all branches
            dirPred.update(pc, taken, dpTrain, mispred);
        end
        // train next addr pred when mispred
        if(mispred) begin
            napTrainByExe.wset(TrainNAP {pc: pc, nextPc: next_pc});
        end
    endmethod

    method FetchDebugState getFetchState;
        return FetchDebugState {
            pc: pc_reg[0],
            waitForRedirect: waitForRedirect,
            waitForFlush: waitForFlush,
            mainEp: f_main_epoch
        };
    endmethod

    interface Perf perf;
        method Action setStatus(Bool stats);
`ifdef PERF_COUNT
            doStats <= stats;
`else
            noAction;
`endif
        endmethod

        method Action req(DecStagePerfType r);
            perfReqQ.enq(r);
        endmethod

        method ActionValue#(PerfResp#(DecStagePerfType)) resp;
`ifdef PERF_COUNT
            perfRespQ.deq;
            return perfRespQ.first;
`else
            perfReqQ.deq;
            return PerfResp {
                pType: perfReqQ.first,
                data: 0
            };
`endif
        endmethod

`ifdef PERF_COUNT
        method Bool respValid = perfRespQ.notEmpty;
`else
        method Bool respValid = perfReqQ.notEmpty;
`endif
    endinterface
endmodule
 
