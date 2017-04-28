
// Copyright (c) 2017 Massachusetts Institute of Technology
// 
// Permission is hereby granted, free of charge, to any person
// obtaining a copy of this software and associated documentation
// files (the "Software"), to deal in the Software without
// restriction, including without limitation the rights to use, copy,
// modify, merge, publish, distribute, sublicense, and/or sell copies
// of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
// 
// The above copyright notice and this permission notice shall be
// included in all copies or substantial portions of the Software.
// 
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
// EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
// MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
// NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
// BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
// ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
// CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.

`include "ProcConfig.bsv"
import Types::*;
import ProcTypes::*;
import DefaultValue::*;
import ConcatReg::*;
import ConfigReg::*;
import Ehr::*;
import Fifo::*;
import Vector::*;
import FIFO::*;
import GetPut::*;

interface CsrFile;
  // Read and Write ports
  method Data rd(CSR csr);
  method Action wr(Maybe#(CSR) csr, Data x, Bit#(5) fflags, Bool fpu_dirty, Bool x_dirty);
  // check whether CSRF needs write (XXX starting value of the cycle of fflags_reg is used here)
  method Bool needWr(Maybe#(CSR) csr, Bit#(5) fflags, Bool fpu_dirty, Bool x_dirty);

  // Methods for handling traps
  method Maybe#(Interrupt) pending_interrupt;
  method ActionValue#(Addr) trap(Trap t, Addr pc, Addr addr);
  method ActionValue#(Addr) sret;
  method ActionValue#(Addr) mrts;

  // Outputs for CSRs that the rest of the processor needs to know about
  method VMInfo vmI;
  method VMInfo vmD;
  method CsrState csrState; // prv, frm, f_enabled, x_enabled

  // Updating specific CSRs outside of normal CSR write instructions
  method Action instret_inc(SupCnt x);

  // tohost/fromhost
  method Action hostToCsrf(Data val);
  method ActionValue#(Data) csrfToHost;
  method Bool toHostEmpty;
  method Bool fromHostZero;

  // inter proc interrupt
  method Action recvIPI;
  method ActionValue#(CoreId) sendIPI;
  method Bool msipZero; // for checking value of msip when recv IPI

  // performance stats is collected or not
  method Bool doPerfStats;

  // terminate
  method ActionValue#(void) terminate;
endinterface

// Fancy Reg functions
function Reg#(Bit#(n)) truncateReg(Reg#(Bit#(m)) r) provisos (Add#(a__,n,m));
  return (interface Reg;
      method Bit#(n) _read = truncate(r._read);
      method Action _write(Bit#(n) x) = r._write({truncateLSB(r._read), x});
    endinterface);
endfunction

function Reg#(Bit#(n)) truncateRegLSB(Reg#(Bit#(m)) r) provisos (Add#(a__,n,m));
  return (interface Reg;
      method Bit#(n) _read = truncateLSB(r._read);
      method Action _write(Bit#(n) x) = r._write({x, truncate(r._read)});
    endinterface);
endfunction

function Reg#(Bit#(n)) zeroExtendReg(Reg#(Bit#(m)) r) provisos (Add#(a__,m,n));
  return (interface Reg;
      method Bit#(n) _read = zeroExtend(r._read);
      method Action _write(Bit#(n) x) = r._write(truncate(x));
    endinterface);
endfunction

function Reg#(t) readOnlyReg(t r);
  return (interface Reg;
      method t _read = r;
      method Action _write(t x) = noAction;
    endinterface);
endfunction
// module version of readOnlyReg for convenience
module mkReadOnlyReg#(t x)(Reg#(t));
  return readOnlyReg(x);
endmodule

function Reg#(t) addWriteSideEffect(Reg#(t) r, Action a);
  return (interface Reg;
      method t _read = r._read;
      method Action _write(t x);
        r._write(x);
        a;
      endmethod
    endinterface);
endfunction

function Bool has_csr_permission(CSR csr, Bit#(2) prv, Bool write);
  Bit#(12) csr_index = pack(csr);
  return ((prv >= csr_index[9:8]) && (!write || (csr_index[11:10] != 2'b11)));
endfunction

interface MToHost;
  interface Reg#(Data)      reg_ifc;
  interface Fifo#(2, Data)  fifo_ifc;
endinterface

module mkMToHost(MToHost);
  Fifo#(2, Data) fifo <- mkCFFifo;

  interface Reg reg_ifc;
    method Action _write(Data x);
      fifo.enq(x);
    endmethod
    method Data _read;
      if (fifo.notEmpty) begin
        return fifo.first;
      end else begin
        return 0;
      end
    endmethod
  endinterface
  interface Fifo fifo_ifc = fifo;
endmodule

interface SendIPI;
    interface Reg#(Data) reg_ifc;
    method ActionValue#(CoreId) sendIPI;
endinterface

module mkSendIPI(SendIPI);
    FIFO#(CoreId) ipiQ <- mkFIFO;
    
    interface Reg reg_ifc;
        method Action _write(Data x);
            if(x < fromInteger(valueof(CoreNum))) begin
                ipiQ.enq(truncate(x));
            end
            else begin
                $fdisplay(stderr, "[SendIPI] WARNING: write %d >= core num %d", x, valueof(CoreNum));
            end
        endmethod
        method Data _read = 0;
    endinterface
    method ActionValue#(CoreId) sendIPI = toGet(ipiQ).get;
endmodule

interface Terminate;
    interface Reg#(Data) reg_ifc;
    method ActionValue#(void) terminate;
endinterface

module mkTerminate(Terminate);
    FIFO#(void) terminateQ <- mkFIFO1;

    interface Reg reg_ifc;
        method Action _write(Data x);
            terminateQ.enq(?);
            $display("[Terminate CSR] being written (val = %d), send terminate signal to host", x);
        endmethod
        method Data _read = 0;
    endinterface

    method terminate = toGet(terminateQ).get;
endmodule

// write ports are serialized, but reads always get stale value
module mkConfigEhr#(t init)(Ehr#(n, t)) provisos(Bits#(t, w));
    Ehr#(n, t) data <- mkEhr(init);
    Vector#(n, Wire#(t)) read <- replicateM(mkBypassWire);

    (* fire_when_enabled, no_implicit_conditions *)
    rule setRead;
        for(Integer i = 0; i < valueOf(n); i = i+1) begin
            read[i] <= data[0];
        end
    endrule

    Ehr#(n, t) ifc = ?;
    for(Integer i = 0; i < valueOf(n); i = i+1) begin
        ifc[i] = (interface Reg;
            method _read = read[i]._read;
            method _write = data[i]._write;
        endinterface);
    end
    return ifc;
endmodule

// TODO can we synthsize this module??
module mkCsrFileWithId#(Data hartid)(CsrFile);
  let mkCsrReg = mkConfigReg; // can use config reg if necessary
  RiscVISASubset isa = defaultValue;
`ifdef LOOK_LIKE_A_ROCKET
  Data mimpid = {'b0, 16'h0001};
`else
  Data mimpid = {'b0, 16'h8000};
`endif

  // Storage elements for CSRs
  //---------------------------
  // User level CSRs
  Reg#(Bit#(5)) fflags_reg  <- mkCsrReg(0);
  Reg#(Bit#(3)) frm_reg     <- mkCsrReg(0);
  // [sizhuo] give config reg behaviors to cycle,time,instret CSRs
  Ehr#(2,Data)  cycle_ehr   <- mkConfigEhr(0); //mkEhr(0);
`ifdef CYCLE_COUNT_EQ_INST_COUNT
  Ehr#(2,Bit#(TAdd#(64,7))) time_ehr <- mkConfigEhr(0); //mkEhr(0); // 7 extra bits so time_reg ticks once every 128 clock cycles
`else
  // slow timer if time is incrementing every cycle
  Ehr#(2,Bit#(TAdd#(64,10))) time_ehr <- mkConfigEhr(0); //mkEhr(0); // 10 extra bits so time_reg ticks once every 1024 clock cycles
`endif
  Ehr#(2,Data)  instret_ehr <- mkConfigEhr(0); //mkEhr(0);
  Reg#(Data)    cycle_reg   =  cycle_ehr[0];
  Reg#(Data)    time_reg    =  truncateRegLSB(time_ehr[0]);
  Reg#(Data)    instret_reg =  instret_ehr[0];
  // Reg#(Data)    cycle_reg   <- mkCsrReg(0);
  // Reg#(Data)    time_reg    <- mkCsrReg(0);
  // Reg#(Data)    instret_reg <- mkCsrReg(0);

  // whether performance stats is collected (only need 1 bit)
  Reg#(Data)    stats_reg   <- mkConfigReg(0); 
  // non-standard: terminate
  Terminate     terminate_module <- mkTerminate;
  Reg#(Data)    terminate_reg    =  terminate_module.reg_ifc;

  // Supervisor level CSRs
  // stvec
  Reg#(Bit#(62)) stvec_reg <- mkCsrReg(0);
  // sie
  // stimecmp
  Reg#(Data) stimecmp_reg <- mkCsrReg(0);
  // sscratch
  Reg#(Data) sscratch_reg <- mkCsrReg(0);
  // sepc
  Reg#(Data) sepc_reg <- mkCsrReg(0);
  // scause
  Reg#(Bit#(1)) scause_i_reg <- mkCsrReg(0);
  Reg#(Bit#(4)) scause_code_reg <- mkCsrReg(0);
  // sbadaddr
  Reg#(Data) sbadaddr_reg <- mkCsrReg(0);
  // sip
  // sptbr
  Reg#(Bit#(52)) sptbr_reg <- mkCsrReg(0);
  // sasid
  Reg#(Asid) sasid_reg <- mkCsrReg(0);

  // Machine level CSRs
  // mstatus
  Reg#(Bit#(5)) vm_reg    <- mkCsrReg(0);
  Reg#(Bit#(1)) mprv_reg  <- mkCsrReg(0);
  Reg#(Bit#(2)) xs_reg    <- isa.x ? mkCsrReg(0) : mkReadOnlyReg(0);
  Reg#(Bit#(2)) fs_reg    <- (isa.f || isa.d) ? mkCsrReg(0) : mkReadOnlyReg(0);
  Reg#(Bit#(1)) sd_reg    =  readOnlyReg( ((xs_reg == 2'b11) || (fs_reg == 2'b11)) ? 1 : 0 );
  Reg#(Bit#(2)) prv3_reg  <- isa.h ? mkCsrReg(0) : mkReadOnlyReg(0);
  Reg#(Bit#(1)) ie3_reg   <- isa.h ? mkCsrReg(0) : mkReadOnlyReg(0);
  Reg#(Bit#(2)) prv2_reg  <- mkCsrReg(0);
  Reg#(Bit#(1)) ie2_reg   <- mkCsrReg(0);
  Reg#(Bit#(2)) prv1_reg  <- mkCsrReg(0);
  Reg#(Bit#(1)) ie1_reg   <- mkCsrReg(0);
  Reg#(Bit#(2)) prv_reg   <- mkCsrReg(3);
  Reg#(Bit#(1)) ie_reg    <- mkCsrReg(0);
  // sstatus (supervisor view of sstatus
  Reg#(Bit#(1)) ps_reg    = truncateReg(prv1_reg);
  Reg#(Bit#(1)) pie_reg   = ie1_reg;
  // mtvec
  Reg#(Bit#(62)) mtvec_reg <- mkCsrReg(62'h40); // bottom two bits will be hardwired to 0
  // mtdeleg
  Reg#(Data) mtdeleg_reg <- mkCsrReg(0);
  // mip
  Reg#(Bit#(1)) mtip_reg <- mkCsrReg(0);
  Reg#(Bit#(1)) htip_reg =  readOnlyReg(0);
  Reg#(Bit#(1)) stip_reg <- mkCsrReg(0);
  Reg#(Bit#(1)) msip_reg <- mkCsrReg(0);
  Reg#(Bit#(1)) hsip_reg =  readOnlyReg(0);
  Reg#(Bit#(1)) ssip_reg <- mkCsrReg(0);
  // mie
  Reg#(Bit#(1)) mtie_reg <- mkCsrReg(0);
  Reg#(Bit#(1)) htie_reg =  readOnlyReg(0);
  Reg#(Bit#(1)) stie_reg <- mkCsrReg(0);
  Reg#(Bit#(1)) msie_reg <- mkCsrReg(0);
  Reg#(Bit#(1)) hsie_reg =  readOnlyReg(0);
  Reg#(Bit#(1)) ssie_reg <- mkCsrReg(0);
  // mtimecmp
  Reg#(Bit#(32)) mtimecmp_reg <- mkCsrReg(0);
  // mtime
  // mscratch
  Reg#(Data)    mscratch_reg <- mkCsrReg(0);
  // mepc
  Reg#(Data)    mepc_reg <- mkCsrReg(0); // Bottom two bits are always 0 on systems where instructions are 32-bit aligned
  // mcause
  Reg#(Bit#(1)) mcause_i_reg <- mkCsrReg(0);
  Reg#(Bit#(4)) mcause_code_reg <- mkCsrReg(0);
  // mbadaddr
  Reg#(Data)    mbadaddr_reg <- mkCsrReg(0);
  // mbase
  Reg#(Data)    mbase_reg <- mkCsrReg(0);
  // mbound
  Reg#(Data)    mbound_reg <- mkCsrReg(0);
  // mibase
  Reg#(Data)    mibase_reg <- mkCsrReg(0);  // sstatus
  // mibound
  Reg#(Data)    mibound_reg <- mkCsrReg(0);
  // mdbase
  Reg#(Data)    mdbase_reg <- mkCsrReg(0);
  // mdbound
  Reg#(Data)    mdbound_reg <- mkCsrReg(0);
  // tohost/fromhost
  // Reg#(Data)    mtohost_reg <- mkCsrReg(0);
  // Reg#(Data)    mfromhost_reg <- mkCsrReg(0);
  // Reg#(Data)    mtohost_reg <- mkConfigReg(0);
  MToHost       mtohost_module <- mkMToHost;
  Reg#(Data)    mtohost_reg = mtohost_module.reg_ifc;
  Reg#(Data)    mfromhost_reg <- mkConfigReg(0);
  // send ipi
  SendIPI       send_ipi_module <- mkSendIPI;
  Reg#(Data)    send_ipi_reg = send_ipi_module.reg_ifc;


  // Write and Read methods that make up CSRs
  //------------------------------------------
  // User level CSRs

  // not this simple -- these need to dirty the fs_reg
  Reg#(Data) fflags_csr   = addWriteSideEffect(zeroExtendReg(fflags_reg),                           fs_reg._write(2'b11));
  Reg#(Data) frm_csr      = addWriteSideEffect(zeroExtendReg(frm_reg),                              fs_reg._write(2'b11));
  Reg#(Data) fcsr_csr     = addWriteSideEffect(concatReg3(readOnlyReg(56'h0), frm_reg, fflags_reg), fs_reg._write(2'b11));

  Reg#(Data) stats_csr    = stats_reg;
  Reg#(Data) terminate_csr= terminate_reg;

  Reg#(Data) cycle_csr    = readOnlyReg(cycle_reg);
  Reg#(Data) time_csr     = readOnlyReg(time_reg);
  Reg#(Data) instret_csr  = readOnlyReg(instret_reg);

  // Supervisor level CSRs
  Reg#(Data) stvec_csr    = concatReg2(stvec_reg, readOnlyReg(2'h0));
  Reg#(Data) sstatus_csr  = concatReg10(sd_reg, readOnlyReg(46'h0), mprv_reg, xs_reg, fs_reg, readOnlyReg(7'h0), ps_reg, pie_reg, readOnlyReg(2'h0), ie_reg);
  Reg#(Data) sip_csr      = concatReg5(readOnlyReg(58'h0), stip_reg, readOnlyReg(3'h0), ssip_reg, readOnlyReg(1'h0));
  Reg#(Data) sie_csr      = concatReg5(readOnlyReg(58'h0), stie_reg, readOnlyReg(3'h0), ssie_reg, readOnlyReg(1'h0));
  Reg#(Data) stime_csr    = time_reg;
  Reg#(Data) stimecmp_csr = addWriteSideEffect(zeroExtendReg(stimecmp_reg), mtip_reg._write(0));
  Reg#(Data) sscratch_csr = sscratch_reg;
  Reg#(Data) sepc_csr     = sepc_reg;
  Reg#(Data) scause_csr   = concatReg3(scause_i_reg, readOnlyReg(59'h0), scause_code_reg);
  Reg#(Data) sbadaddr_csr = sbadaddr_reg;
  Reg#(Data) sptbr_csr    = concatReg2(sptbr_reg, readOnlyReg(12'h0));
  Reg#(Data) sasid_csr    = zeroExtendReg(sasid_reg);
  Reg#(Data) cyclew_csr   = cycle_reg;
  Reg#(Data) timew_csr    = time_reg;
  Reg#(Data) instretw_csr = instret_reg;

  // Machine level CSRs
  Reg#(Data) mcpuid_csr   = readOnlyReg(getMCPUID(isa));
  Reg#(Data) mimpid_csr   = readOnlyReg(mimpid);
  Reg#(Data) mhartid_csr  = readOnlyReg(hartid);
  Reg#(Data) mstatus_csr  = concatReg14(sd_reg, readOnlyReg(41'h0), vm_reg, mprv_reg, xs_reg, fs_reg, prv3_reg, ie3_reg, prv2_reg, ie2_reg, prv1_reg, ie1_reg, prv_reg, ie_reg);
  Reg#(Data) mtvec_csr    = concatReg2(mtvec_reg, readOnlyReg(2'h0));
  Reg#(Data) mtdeleg_csr  = mtdeleg_reg;
  Reg#(Data) mip_csr      = concatReg9(readOnlyReg(56'h0), mtip_reg, htip_reg, stip_reg, readOnlyReg(1'h0), msip_reg, hsip_reg, ssip_reg, readOnlyReg(1'h0));
  Reg#(Data) mie_csr      = concatReg9(readOnlyReg(56'h0), mtie_reg, htie_reg, stie_reg, readOnlyReg(1'h0), msie_reg, hsie_reg, ssie_reg, readOnlyReg(1'h0));
  Reg#(Data) mtime_csr    = time_reg;
  Reg#(Data) mtimecmp_csr = zeroExtendReg(mtimecmp_reg);
  Reg#(Data) mscratch_csr = mscratch_reg;
  Reg#(Data) mepc_csr     = mepc_reg;
  Reg#(Data) mcause_csr   = concatReg3(mcause_i_reg, readOnlyReg(59'h0), mcause_code_reg);
  Reg#(Data) mbadaddr_csr = mbadaddr_reg;
  Reg#(Data) mtohost_csr  = mtohost_reg;
  Reg#(Data) mfromhost_csr= mfromhost_reg;
  Reg#(Data) mbase_csr    = mbase_reg;
  Reg#(Data) mbound_csr   = mbound_reg;
  Reg#(Data) mibase_csr   = mibase_reg;
  Reg#(Data) mibound_csr  = mibound_reg;
  Reg#(Data) mdbase_csr   = mdbase_reg;
  Reg#(Data) mdbound_csr  = mdbound_reg;

  // inter proc interrupt CSR
  Reg#(Data) send_ipi_csr  = send_ipi_reg;

  // Function for getting a csr given an index
  function Reg#(Data) get_csr(CSR csr);
    return (case (csr)
        // User Floating-Point CSRs
        CSRfflags:    fflags_csr;
        CSRfrm:       frm_csr;
        CSRfcsr:      fcsr_csr;
        // User stats
        CSRstats:     stats_csr;
        // User Counter/Timers
        CSRcycle:     cycle_csr;
        CSRtime:      time_csr;
        CSRinstret:   instret_csr;
        // User Terminate
        CSRterminate: terminate_csr;

        // Supervisor Trap Setup
        CSRsstatus:   sstatus_csr;
        CSRstvec:     stvec_csr;
        CSRsie:       sie_csr;
        CSRstimecmp:  stimecmp_csr;
        // Supervisor Timer
        CSRstime:     stime_csr;
        // Supervisor Trap Handling
        CSRsscratch:  sscratch_csr;
        CSRsepc:      sepc_csr;
        CSRscause:    scause_csr;
        CSRsbadaddr:  sbadaddr_csr;
        CSRsip:       sip_csr;
        // Supervisor Protection and Translation
        CSRsptbr:     sptbr_csr;
        CSRsasid:     sasid_csr;
        // Supervisor Read/Write Shadow of User Read-Only registers
        CSRcyclew:    cyclew_csr;
        CSRtimew:     timew_csr;
        CSRinstretw:  instretw_csr;

        // Machine Information Registers
        CSRmcpuid:    mcpuid_csr;
        CSRmimpid:    mimpid_csr;
        CSRmhartid:   mhartid_csr;
        // Machine Trap Setup
        CSRmstatus:   mstatus_csr;
        CSRmtvec:     mtvec_csr;
        CSRmtdeleg:   mtdeleg_csr;
        CSRmie:       mie_csr;
        CSRmtimecmp:  mtimecmp_csr;
        // Machine Timers and Counters
        CSRmtime:     mtime_csr;
        // Machine Trap Handling
        CSRmscratch:  mscratch_csr;
        CSRmepc:      mepc_csr;
        CSRmcause:    mcause_csr;
        CSRmbadaddr:  mbadaddr_csr;
        CSRmip:       mip_csr;
        // Machine Protection and Translation
        CSRmbase:     mbase_csr;
        CSRmbound:    mbound_csr;
        CSRmibase:    mibase_csr;
        CSRmibound:   mibound_csr;
        CSRmdbase:    mdbase_csr;
        CSRmdbound:   mdbound_csr;
        // Machine Read/Write Shadow of Hypervisor Read-Only Registers
        // TODO: implement
        // Machine Host-Target Interface (Non-Standard Berkeley Extension)
        CSRmtohost:   mtohost_csr;
        CSRmfromhost: mfromhost_csr;
        // send IPI
        CSRsend_ipi: send_ipi_csr;

        default:      (readOnlyReg(64'h0));
      endcase);
  endfunction

  rule updateMTIP(truncate(time_csr) >= mtimecmp_reg);
    mtip_reg <= 1;
  endrule

`ifndef DISABLE_STIP
  rule updateSTIP(truncate(stime_csr) >= stimecmp_reg);
    stip_reg <= 1;
  endrule
`endif

`ifndef CYCLE_COUNT_EQ_INST_COUNT
  rule incrementTimeAndCycle;
    time_ehr[1] <= time_ehr[1] + 1;
    cycle_ehr[1] <= cycle_ehr[1] + 1;
  endrule
`endif

  method Data rd(CSR csr);
    // if (!has_csr_permission(csr, prv_reg, False)) return 0;
    // else
    return get_csr(csr)._read;
  endmethod

  method Action wr(Maybe#(CSR) csr, Data x, Bit#(5) fflags, Bool fpu_dirty, Bool x_dirty);
    Bool fflags_conflict = False; // writing to fflags or fcsr
    Bool fs_conflict = False;
    Bool xs_conflict = False;
    // update fpu_dirty if fflags will change the fflags csr
    if ((fflags & fflags_reg) != fflags) begin
        fpu_dirty = True;
    end

    case (csr) matches
      tagged Valid CSRfflags:
        begin
          fflags_conflict = True;
          fs_conflict = True;
        end
      tagged Valid CSRfrm: fs_conflict = True;
      tagged Valid CSRfcsr:
        begin
          fflags_conflict = True;
          fs_conflict = True;
        end
      tagged Valid CSRsstatus:
        begin
          fs_conflict = True;
          xs_conflict = True;
        end
      tagged Valid CSRmstatus:
        begin
          fs_conflict = True;
          xs_conflict = True;
        end
    endcase

    if (isValid(csr)) begin
      get_csr(fromMaybe(?,csr))._write(x);
    end
    if (!fflags_conflict) begin
      fflags_reg <= fflags_reg | fflags;
    end
    if (fpu_dirty && !fs_conflict) begin
      fs_reg <= 2'b11;
    end
    if (x_dirty && !xs_conflict) begin
      xs_reg <= 2'b11;
    end
  endmethod

  method Bool needWr(Maybe#(CSR) csr, Bit#(5) fflags, Bool fpu_dirty, Bool x_dirty);
      return isValid(csr) || (fflags & fflags_reg) != fflags || fpu_dirty || x_dirty;
  endmethod

  method Maybe#(Interrupt) pending_interrupt;
    Maybe#(Interrupt) ret = tagged Invalid;
    Bool ie = ie_reg == 1;
    if (prv_reg < prvM || ((prv_reg == prvM) && ie)) begin
      if ((msie_reg & msip_reg) == 1) begin
        ret = tagged Valid SoftwareInterrupt;
      end else if ((mtie_reg & mtip_reg) == 1) begin
        ret = tagged Valid TimerInterrupt;
      end else if (mfromhost_reg != 0) begin
        ret = tagged Valid HostInterrupt;
      end
    end
    if (!isValid(ret) && (prv_reg < prvS || ((prv_reg == prvS) && ie))) begin
      if ((ssie_reg & ssip_reg) == 1) begin
        ret = tagged Valid SoftwareInterrupt;
      end else if ((stie_reg & stip_reg) == 1) begin
        ret = tagged Valid TimerInterrupt;
      end
    end
    return ret;
  endmethod

  method ActionValue#(Addr) trap(Trap t, Addr pc, Addr addr);
    // Check mtdeleg
    Bit#(1) deleg_bit = (case (t) matches
        tagged Exception .x: mtdeleg_csr[pack(x)];
        tagged Interrupt .x: mtdeleg_csr[pack(x) << 4];
      endcase);
    // Disable mprv
    mprv_reg <= 0;
    // push stack
    prv3_reg <= prv2_reg; // no action if H-mode isn't supported
    ie3_reg <= ie2_reg;   // no action if H-mode isn't supported
    prv2_reg <= prv1_reg;
    ie2_reg <= ie1_reg;
    prv1_reg <= prv_reg;
    ie1_reg <= ie_reg;
    Addr next_pc = 0;
    if (deleg_bit == 1) begin
      // trap to S-mode
      prv_reg <= prvS;
      ie_reg <= 0;
      sepc_reg <= pc;
      case (t) matches
        tagged Exception .x:
          begin
            scause_i_reg <= 0;
            scause_code_reg <= pack(x);
            case (x)
              InstAddrMisaligned, InstAccessFault:
                sbadaddr_reg <= pc;
              LoadAddrMisaligned, LoadAccessFault, StoreAddrMisaligned, StoreAccessFault:
                sbadaddr_reg <= addr;
            endcase
          end
        tagged Interrupt .x:
          begin
            scause_i_reg <= 1;
            scause_code_reg <= pack(x);
          end
      endcase
      next_pc = stvec_csr + (zeroExtend(prv_reg) << 6);
    end else begin
      // trap to M-mode
      prv_reg <= prvM;
      ie_reg <= 0;
      mepc_reg <= pc;
      case (t) matches
        tagged Exception .x:
          begin
            mcause_i_reg <= 0;
            mcause_code_reg <= pack(x);
            case (x)
              InstAddrMisaligned, InstAccessFault:
                mbadaddr_reg <= pc;
              LoadAddrMisaligned, LoadAccessFault, StoreAddrMisaligned, StoreAccessFault:
                mbadaddr_reg <= addr;
            endcase
          end
        tagged Interrupt .x:
          begin
            mcause_i_reg <= 1;
            mcause_code_reg <= pack(x);
          end
      endcase
      next_pc = mtvec_csr + (zeroExtend(prv_reg) << 6);
    end
    // XXX yield load reservation is done outside when calling this method
    return next_pc;
  endmethod

  method ActionValue#(Addr) sret;
    // pop stack
    prv_reg <= prv1_reg;
    ie_reg <= ie1_reg;
    prv1_reg <= prv2_reg;
    ie1_reg <= ie2_reg;
    if (isa.h) begin
      prv2_reg <= prv3_reg;
      ie2_reg <= ie3_reg;
      prv3_reg <= prvU;
      ie3_reg <= 1;
    end else begin
      prv2_reg <= prvU;
      ie2_reg <= 1;
    end
    let next_pc = 0;
    if (prv_reg == prvS) begin
      next_pc = sepc_csr;
    end else if (prv_reg == prvM) begin
      next_pc = mepc_csr;
    end else begin
      $fdisplay(stderr, "[ERROR] CsrFile: sret called when processor wasn't in S or M mode");
      $finish;
    end
    return next_pc;
  endmethod

  method ActionValue#(Addr) mrts;
    prv_reg <= prvS;
    sbadaddr_reg <= mbadaddr_reg;
    scause_i_reg <= mcause_i_reg;
    scause_code_reg <= mcause_code_reg;
    sepc_reg <= mepc_reg;
    return stvec_csr;
  endmethod

  method VMInfo vmI;
    Bit#(2) prv = prv_reg;
    Asid asid = sasid_reg;
    Bit#(5) vm = (prv == prvM) ? vmMbare : vm_reg;
    Addr base = 0;
    Addr bound = 0;
    case (vm)
      vmMbare:
        begin
          base = 0;
          bound = -1;
        end
      vmMbb:
        begin
          base = mbase_csr;
          bound = mbound_csr;
        end
      vmMbbid:
        begin
          base = mibase_csr;
          bound = mibound_csr;
        end
      vmSv32, vmSv39, vmSv48, vmSv57, vmSv64:
        begin
          base = sptbr_csr;
          bound = -1;
        end
    endcase
    return VMInfo{ prv: prv, asid: asid, vm: vm, base: base, bound: bound };
  endmethod

  method VMInfo vmD;
    Bit#(2) prv = (mprv_reg == 1) ? prv1_reg : prv_reg;
    Asid asid = sasid_reg;
    Bit#(5) vm = (prv == prvM) ? vmMbare : vm_reg;
    Addr base = 0;
    Addr bound = 0;
    case (vm)
      vmMbare:
        begin
          base = 0;
          bound = -1;
        end
      vmMbb:
        begin
          base = mbase_csr;
          bound = mbound_csr;
        end
      vmMbbid:
        begin
          base = mdbase_csr;
          bound = mdbound_csr;
        end
      vmSv32, vmSv39, vmSv48, vmSv57, vmSv64:
        begin
          base = sptbr_csr;
          bound = -1;
        end
    endcase
    return VMInfo{ prv: prv, asid: asid, vm: vm, base: base, bound: bound };
  endmethod

  method CsrState csrState = CsrState {prv: prv_reg, frm: frm_reg, f_enabled: (fs_reg != 0), x_enabled: (xs_reg != 0)};

  // Updating CSRs
  method Action instret_inc(SupCnt x);
    instret_ehr[1] <= instret_ehr[1] + zeroExtend(x);
`ifdef CYCLE_COUNT_EQ_INST_COUNT
    time_ehr[1] <= time_ehr[1] + zeroExtend(x);
    cycle_ehr[1] <= cycle_ehr[1] + zeroExtend(x);
`endif
  endmethod

  // tohost/fromhost
  method Action hostToCsrf(Data val) if (mfromhost_reg == 0);
    mfromhost_reg <= val;
  endmethod
  method ActionValue#(Data) csrfToHost;
    mtohost_module.fifo_ifc.deq;
    return mtohost_module.fifo_ifc.first;
  endmethod
  method Bool toHostEmpty;
    return !mtohost_module.fifo_ifc.notEmpty;
  endmethod
  method Bool fromHostZero;
    return mfromhost_reg == 0;
  endmethod

  method Action recvIPI;
      msip_reg <= 1; // spike does not wait for misp == 0
  endmethod
  method ActionValue#(CoreId) sendIPI = send_ipi_module.sendIPI;
  method Bool msipZero;
      return msip_reg == 0;
  endmethod

  method terminate = terminate_module.terminate;

  // performance stats
  method Bool doPerfStats;
    return stats_reg != 0;
  endmethod
endmodule

// this is single core version
// (* synthesize *)
//module mkCsrFile(CsrFile);
//    let m <- mkCsrFileWithId(0);
//    return m;
//endmodule
