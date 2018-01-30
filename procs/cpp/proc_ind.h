
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

#pragma once

#include <stdio.h>
#include <signal.h>
#include <assert.h>
#include <functional>
#include <mutex>
#include <atomic>
#include <thread>
#include <signal.h>
#include <semaphore.h>
#include "ProcIndication.h"
#include "ProcRequest.h"
#include "GeneratedTypes.h"
#include "print_buff.h"
#include "PerfStats.h"
#include "htif_riscy.h"

class ProcIndication : public ProcIndicationWrapper {
protected:
    const uint32_t core_num;

    // done & result
    int result;
    bool done;

    // tandem verify (not implemented)
    bool error_found;

    // logging
    FILE *debug_file;
    PrintBuffer *print_buff;

    std::mutex proc_mu; // protect all above: result, tandem verify, logging
    
    // notify done signals
    sem_t sem; 
    sem_t reset_sem;
    sem_t bootrom_sem;

    // htif (it has its own lock)
    htif_riscy_t *riscy_htif;

    // performance
    PerfStats *perf_stats;
    
    // handle ctrl-C
    sighandler_t handle_signal;

    // inner class to handle to host msg
    class ToHostHandler {
    private:
        // shared Q of msg
        std::queue<uint64_t> to_host_msg_q;
        std::mutex msg_q_mu;
        std::atomic<int64_t> msg_q_size;

        std::thread handler_thread;
        void handler();

        ProcIndication *proc_ind; // ptr to outer class

    public:
        ToHostHandler(ProcIndication *p_ind);
        ~ToHostHandler();

        void enq_to_host_msg(uint64_t v);
        void spawn_handler_thread();
    };
    ToHostHandler to_host_handler;

    // memory size
    const size_t mem_sz;
    // htif arguments
    std::vector<std::string> *htif_args;

    // shell cmd to be sent to processor
    char *shell_cmd;
    int cmd_delay_sec;

public:
    ProcIndication(
        unsigned int id,
        uint32_t core_cnt,
        FILE *dbg_f,
        PrintBuffer *buff,
        PerfStats *perf,
        sighandler_t sig_hdl,
        size_t mem_size,
        char *cmd,
        int cmd_delay
    ) : ProcIndicationWrapper(id),
        core_num(core_cnt),
        result(-1),
        done(false),
        error_found(false),
        debug_file(dbg_f),
        print_buff(buff),
        riscy_htif(0),
        perf_stats(perf),
        handle_signal(sig_hdl),
        to_host_handler(this),
        mem_sz(mem_size),
        htif_args(0),
        shell_cmd(cmd),
        cmd_delay_sec(cmd_delay)
    {
        std::lock_guard<std::mutex> lock(proc_mu);
        sem_init(&sem, 0, 0);
        sem_init(&reset_sem, 0, 0);
        sem_init(&bootrom_sem, 0, 0);
    }

    virtual ~ProcIndication();

    void set_riscy_htif(htif_riscy_t *p) { riscy_htif = p; }
    void set_htif_args(std::vector<std::string> *p) { htif_args = p; }
    virtual void spawn_to_host_handler() {
        to_host_handler.spawn_handler_thread();
    }

    int waitResult();
    void waitReset();
    void waitBootRomInit();
    virtual void resetDone();
    virtual void debug_verify(uint8_t core, VerificationPacket packet) {
        fprintf(stderr, "Verification not implemented\n");
    }
    virtual void to_host(uint64_t v);
    virtual void bootRomInitResp();
    virtual void perfResp(uint8_t core, ProcPerfResp resp);
    virtual void terminate(uint8_t core);
};

class ProcIndicationAssembly : public ProcIndication {
public:
    virtual void to_host(uint64_t v);
    virtual void spawn_to_host_handler() {} // don't need this handler thread

    ProcIndicationAssembly(
        unsigned int id,
        uint32_t core_cnt,
        FILE *dbg_f,
        PrintBuffer *buff,
        PerfStats *perf,
        sighandler_t sig_hdl,
        size_t mem_size,
        char *cmd,
        int cmd_delay
    ) : ProcIndication(id, core_cnt, dbg_f, buff, perf,
                       sig_hdl, mem_size, cmd, cmd_delay) {}
};

