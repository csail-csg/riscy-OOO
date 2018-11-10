
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

    // htif (it has its own lock)
    htif_riscy_t *riscy_htif;

    // number of cores
    uint32_t core_num;

    // performance
    CorePerfStats *core_perf_stats;
    UncorePerfStats *uncore_perf_stats;
    
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

    // shell cmd to be sent to processor
    char *shell_cmd;
    int cmd_delay_sec;

public:
    ProcIndication(
        unsigned int id,
        FILE *dbg_f,
        PrintBuffer *buff,
        uint32_t cores,
        CorePerfStats *core_perf,
        UncorePerfStats *uncore_perf,
        sighandler_t sig_hdl,
        char *cmd,
        int cmd_delay
    ) : ProcIndicationWrapper(id),
        result(-1),
        done(false),
        error_found(false),
        debug_file(dbg_f),
        print_buff(buff),
        riscy_htif(0),
        core_num(cores),
        core_perf_stats(core_perf),
        uncore_perf_stats(uncore_perf),
        handle_signal(sig_hdl),
        to_host_handler(this),
        shell_cmd(cmd),
        cmd_delay_sec(cmd_delay)
    {
        std::lock_guard<std::mutex> lock(proc_mu);
        sem_init(&sem, 0, 0);
    }

    virtual ~ProcIndication();

    void set_riscy_htif(htif_riscy_t *p) { riscy_htif = p; }

    virtual void spawn_to_host_handler() {
        to_host_handler.spawn_handler_thread();
    }

    int waitResult();

    virtual void to_host(uint64_t v);
    virtual void perfResp(uint8_t core, ProcPerfResp resp);
    virtual void terminate(uint8_t core);
};

class ProcIndicationAssembly : public ProcIndication {
public:
    virtual void to_host(uint64_t v);
    virtual void spawn_to_host_handler() {} // don't need this handler thread

    ProcIndicationAssembly(
        unsigned int id,
        FILE *dbg_f,
        PrintBuffer *buff,
        uint32_t cores,
        CorePerfStats *core_perf,
        UncorePerfStats *uncore_perf,
        sighandler_t sig_hdl,
        char *cmd,
        int cmd_delay
    ) : ProcIndication(id,
                       dbg_f,
                       buff,
                       cores,
                       core_perf,
                       uncore_perf,
                       sig_hdl,
                       cmd,
                       cmd_delay) {}
};

