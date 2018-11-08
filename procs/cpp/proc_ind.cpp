
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

#include <stdlib.h>
#include "proc_ind.h"
#include "fesvr/term.h"

// ProcIndication
ProcIndication::~ProcIndication() {
}

int ProcIndication::waitResult() {
    // ptr to send shell cmd
    int cmd_ptr = 0;
    // delay loops for cmd (each loop takes about 1ms)
    int cmd_delay_loops = cmd_delay_sec * 1000;

    while (sem_trywait(&sem) != 0) {
        {
            std::lock_guard<std::mutex> lock(proc_mu);
            // read stdin if necessary. We need to lock htif, since
            // to_host_handler may also access htif.
            riscy_htif->lock();
            if(riscy_htif->bcd_wait_for_stdin()) {
                // the character to be sent to processor, -1 means nothing
                int ch = -1;
                // get the character to send
                if(shell_cmd) {
                    // read from stored shell cmd
                    if(cmd_delay_loops > 0) {
                        // still waiting for linux to boot...
                        cmd_delay_loops--;
                    } else {
                        // send one character from the shell cmd
                        if(shell_cmd[cmd_ptr] != 0) {
                            ch = shell_cmd[cmd_ptr];
                            cmd_ptr++;
                        } else {
                            // all chars of the cmd is sent, now send a '\n' to
                            // execute the cmd we are done with stored shell
                            // cmd
                            ch = '\n';
                            shell_cmd = 0;
                        }
                    }
                } else {
                    // read from keyboard
                    ch = canonical_terminal_t::read();
                }
                // send character to processor via htif
                if(ch != -1) {
                    riscy_htif->bcd_feed_stdin(ch);
                }
            }
            riscy_htif->unlock();
        }
        usleep(1000); // delay 1ms
    }
    return result;
}

void ProcIndication::to_host(uint64_t v) {
    fprintf(debug_file, "[to_host] value 0x%016llx\n", (long long) v);
    to_host_handler.enq_to_host_msg(v);
}

void ProcIndication::perfResp(uint8_t core, ProcPerfResp resp) {
    if(uint32_t(core) >= core_num) {
        uncore_perf_stats->inform_resp(core, resp.loc, resp.pType, resp.data);
    }
    else {
        core_perf_stats->inform_resp(core, resp.loc, resp.pType, resp.data);
    }
}

void ProcIndication::terminate(uint8_t core) {
    {
        std::lock_guard<std::mutex> lock(proc_mu);
        if (error_found) {
            // print output_buff
            print_buff->flush_print();
        }
        fprintf(stderr, "\nCore %d signaled terminate\n", (int)core);
        result = 0; // result default to 0 (success val)
        done = true;
    }
    sem_post(&sem); // notify program finish
}

// ProcIndicationAssembly
void ProcIndicationAssembly::to_host(uint64_t v) {
    {
        std::lock_guard<std::mutex> lock(proc_mu);
        if (error_found) {
            // print output_buff
            print_buff->flush_print();
        }
        // don't bother passing it to the htif
        // the only thing that goes in to_host are exit codes
        if (v == 1) {
            fprintf(stderr, "[32;1mPASSED[0m\n");
        } else {
            fprintf(stderr, "[31;1mFAILED %lld[0m\n", (long long)(v >> 1));
        }
        result = v >> 1;
        done = true;
    }
    sem_post(&sem);
}

