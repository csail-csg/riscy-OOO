
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

#include <errno.h>
#include <stdio.h>
#include <cstring>
#include <cassert>
#include <fcntl.h>
#include <string.h>
#include <iostream>
#include <sys/stat.h>
#include <unistd.h>
#include <vector>
#include <string>
#include <sstream>
#include <list>
#include <signal.h>
#include "PerfStats.h"
#include "print_buff.h"
#include "proc_ind.h"
#include "host_dma.h"
#include "DeadlockRequest.h"
#include "deadlock.h"
#include "rename_debug.h"

#ifdef NDEBUG
#error fesvr will not work with NDEBUG defined
#endif

#define BLURT fprintf (stderr, "CPPDEBUG: %s(%s):%d\n",\
                      __func__, __FILE__, __LINE__)

// system config
static uint32_t core_num = 1; // default to 1 core

// File for output
static FILE *debug_file = 0;
static PrintBuffer *print_buff = 0;
FILE *perf_fp = stderr; // performance counters output file

// proc requests & indications
static ProcRequestProxy *procRequestProxy = 0;
static ProcIndication   *procIndication = 0;
static PerfStats perf_stats;

// tandem verify params
static uint64_t verification_packets_skipped = 0;
static uint64_t verification_packets_printed = 0;
static bool synchronization_packets_sent = true;

// host dma request & indications
static HostDmaRequestProxy *hostDmaRequestProxy = 0;
static HostDmaIndication *hostDmaIndication = 0;

// deadlock detect
static DeadlockRequestProxy *deadlockReqeustProxy = 0;
static DeadlockIndication *deadlockIndication = 0;
static long long unsigned deadlock_check_start_inst_num = 0;

// rename debug
static RenameDebugIndication *renameDebugIndication = 0;

static size_t mem_sz = 64*1024*1024;  // default 64 MB memory

// HTIF
static std::vector<std::string> htif_args;
static htif_riscy_t *riscy_htif = NULL;

// call back functions, XXX these functions should not be called directly!!
static void handle_signal(int sig) {
    // print out stderr before exiting
    fprintf(stderr, "\n>> Ctrl-C: Exiting... Print buffered stderr\n");
    print_buff->flush_print(); // don't lock here, otherwise may deadlock
    fclose(debug_file);
    exit(1);
}
static void call_from_host(uint64_t v) {
    fprintf(debug_file, "[from_host] val 0x%016llx\n", (long long) v);
    procRequestProxy->from_host(v);
}
static void call_dma_read(addr_t addr, size_t len, void *dst) {
    hostDmaIndication->dma_read(addr, len, dst);
}
static void call_dma_write(addr_t addr, size_t len, const void *src) {
    hostDmaIndication->dma_write(addr, len, src);
}
static void call_boot_rom_write(uint16_t index, uint64_t data) {
    procRequestProxy->bootRomInitReq(index, data);
}
static void call_boot_rom_wait() {
    procIndication->waitBootRomInit();
}
static void req_perf_counter(uint8_t core, PerfLocation loc, PerfType type) {
    procRequestProxy->perfReq(core, loc, type);
}

int runHtifTest() {
    // reset the processor before programming memory
    // prevent uncommitted store from polluting memory
    // XXX [sizhuo] this actually do nothing now..
    fprintf(stderr, "resetting the processor\n");
    procRequestProxy->reset();
    procIndication->waitReset();
    sleep(1);

    // program the memory
    fprintf(stderr, "programming the memory\n");
    riscy_htif->lock();
    riscy_htif->start();
    riscy_htif->unlock();
    sleep(1);

    // set deadlock start inst num
    fprintf(stderr, "set deadlock check starting after inst num %llu\n",
            deadlock_check_start_inst_num);
    deadlockReqeustProxy->setCheckStartInstNum(
            (uint64_t)deadlock_check_start_inst_num);
    sleep(1);

    // start processor
    uint64_t startpc = 0x1000;
    addr_t tohost_addr = riscy_htif->get_tohost_addr();
    addr_t fromhost_addr = riscy_htif->get_fromhost_addr();
    fprintf(stderr, "startpc %llx, total %d cores, "
            "toHost addr %llx, fromHost addr %llx\n",
            (long long unsigned)startpc, (int)core_num,
            (long long unsigned)tohost_addr, (long long unsigned)fromhost_addr);
    procRequestProxy->start(startpc,
                            tohost_addr, fromhost_addr,
                            verification_packets_skipped,
                            synchronization_packets_sent);

    // wait for result
    int result = procIndication->waitResult();

    // performance result
    for(uint32_t i = 0; i < core_num; i++) {
        perf_stats.reset(); // clear old data
        fprintf(perf_fp, "getting performance results for core %d\n", i);
        perf_stats.get_all_perf(i);
        perf_stats.print(perf_fp);
        fprintf(perf_fp, "\n");
    }
    if(perf_fp != stderr && perf_fp != stdout) {
        fclose(perf_fp);
    }

    return result;
}

void printHelp(const char *prog) {
    fprintf(stderr, "Usage: %s [--assembly-tests] ", prog);
    fprintf(stderr, "[--just-run] [--mem-size MEM_MB_SIZE] ");
    fprintf(stderr, "[--deadlock-check-after INST_NUM] ");
    fprintf(stderr, "[--core-num CORE_NUM] ");
    fprintf(stderr, "[--print-from INST_COUNT] [--skip INST_COUNT] ");
    fprintf(stderr, "[--shell-cmd CMD DELAY(sec)] ");
    fprintf(stderr, "[--perf-file PERF_OUTPUT_FILE] ");
    fprintf(stderr, "-- HTIF_ARGS\n");
}

int main(int argc, char * const *argv) {
    debug_file = fopen("proc_debug.txt", "w");
    print_buff = new PrintBuffer(debug_file);

    // parse arguments
    const char *prog_name = argv[0];
    bool assembly_test_mode = false;
    //bool print_mode = false;
    char *shell_cmd = 0;
    int cmd_delay_sec = 0;

    while(1) {
        if (argc > 1 && strcmp(argv[1],"--assembly-tests") == 0) {
            // first argument was "--assembly-tests"
            // this uses an alternate handler for tohost messages ...
            assembly_test_mode = true;
            // shift argc and argv accordingly
            argc--; argv++;
        } else if (argc > 2 && strcmp(argv[1],"--print-from") == 0) {
            // first argument was "--print-from"
            // this enables printing the trace starting at the next argument
            verification_packets_skipped = (uint64_t) atoi(argv[2]);
            verification_packets_printed = 100000;
            //print_mode = true;
            // shift argc and argv accordingly
            argc-=2; argv+=2;
        } else if (argc > 2 && strcmp(argv[1],"--skip") == 0) {
            // first argument was "--skip"
            // this enables skipping some verification packets
            verification_packets_skipped = (uint64_t) atoi(argv[2]);
            // shift argc and argv accordingly
            argc-=2; argv+=2;
        } else if (argc > 1 && strcmp(argv[1],"--just-run") == 0) {
            verification_packets_skipped = (uint64_t) -1;
            synchronization_packets_sent = false;
            argc-=1; argv+=1;
        } else if (argc > 2 && strcmp(argv[1],"--mem-size") == 0) {
            // first argument was "--mem-size"
            // second argument should be memory size in MB
            mem_sz = (size_t)(atoi(argv[2])) * 1024 * 1024;
            // shift argc and argv accordingly
            argc-=2; argv+=2;
        } else if(argc > 2 && strcmp(argv[1],"--deadlock-check-after") == 0) {
            // first argument was "--deadlock-check-after"
            // second argument should be inst num
            deadlock_check_start_inst_num = std::stoull(argv[2]);
            // shift argc and argv accordingly
            argc-=2; argv+=2;
        } else if(argc > 2 && strcmp(argv[1],"--core-num") == 0) {
            // first argument was "--core-num"
            // second argument should be core num
            core_num = std::stoul(argv[2]);
            // shift argc and argv accordingly
            argc-=2; argv+=2;
        } else if(argc > 3 && strcmp(argv[1],"--shell-cmd") == 0) {
            // first argument was "--shell-cmd"
            // the next two args should be CMD and DELAY
            // we will send CMD to processor at DELAY seconds after the
            // processor requests for stdin (wait for linux to boot completely)
            shell_cmd = argv[2];
            cmd_delay_sec = atoi(argv[3]);
            // shift argc and argv accordingly
            argc-=3; argv+=3;
        } else if(argc > 2 && strcmp(argv[1],"--perf-file") == 0) {
            // first argumnet was "--perf-file"
            // second argument should be file name
            perf_fp = fopen(argv[2], "wt");
            if(!perf_fp) {
                fprintf(stderr, "[ERROR] fail to open %s\n", argv[2]);
                return 0;
            }
            // shift argc and argv accordingly
            argc-=2; argv+=2;
        } else if(argc > 1 && strcmp(argv[1],"--") == 0) {
            // first argument was "--"
            // remaining arguments are HTIF argumemts
            // stop parsing runtime parameters
            // shift argc and argv accordingly
            argc-=1; argv+=1;
            break;
        } else {
            fprintf(stderr, "Wrong arguments\n");
            printHelp(prog_name);
            return 0;
        }
    }

#ifdef SIMULATION // safe to always do this, but it's only useful for simulation
    char socket_name[128];
    snprintf(socket_name, sizeof(socket_name), "SOCK.%d", getpid());
    setenv("BLUESIM_SOCKET_NAME", socket_name, 0);
    setenv("SOFTWARE_SOCKET_NAME", socket_name, 0);
#endif

    if (assembly_test_mode) {
        procIndication = new ProcIndicationAssembly(IfcNames_ProcIndicationH2S,
                                                    core_num,
                                                    debug_file,
                                                    print_buff,
                                                    &perf_stats,
                                                    &handle_signal,
                                                    mem_sz,
                                                    shell_cmd,
                                                    cmd_delay_sec);
    } else {
        procIndication = new ProcIndication(IfcNames_ProcIndicationH2S,
                                            core_num, debug_file, print_buff,
                                            &perf_stats, &handle_signal,
                                            mem_sz, shell_cmd, cmd_delay_sec);
    }
    procRequestProxy = new ProcRequestProxy(IfcNames_ProcRequestS2H);

    hostDmaIndication = new HostDmaIndication(IfcNames_HostDmaIndicationH2S);
    hostDmaRequestProxy = new HostDmaRequestProxy(IfcNames_HostDmaRequestS2H);

    deadlockIndication = new DeadlockIndication(IfcNames_DeadlockIndicationH2S);
    deadlockReqeustProxy = new DeadlockRequestProxy(IfcNames_DeadlockRequestS2H);

    renameDebugIndication = new RenameDebugIndication(IfcNames_RenameDebugIndicationH2S);

    // all arguments passed to htif at once
    fprintf(stderr, "htif_args: ");
    for (int i = 1 ; i < argc ; i++ ) {
        htif_args.push_back(argv[i]);
        fprintf(stderr, "%s", argv[i]);
        if (i == argc-1) {
            fprintf(stderr, "\n");
        } else {
            fprintf(stderr, ", ");
        }
    }
    // print arguments to debug_file also
    fprintf(debug_file, "htif_args: ");
    for (int i = 1 ; i < argc ; i++ ) {
        htif_args.push_back(argv[i]);
        fprintf(debug_file, "%s", argv[i]);
        if (i == argc-1) {
            fprintf(debug_file, "\n");
        } else {
            fprintf(debug_file, ", ");
        }
    }
    // create htif
    riscy_htif = new htif_riscy_t(htif_args, core_num);
    riscy_htif->set_mem_size(mem_sz);
    riscy_htif->set_dma_read(call_dma_read);
    riscy_htif->set_dma_write(call_dma_write);
    riscy_htif->set_write_from_host(call_from_host);
    riscy_htif->set_write_boot_rom(call_boot_rom_write);
    riscy_htif->set_wait_boot_rom(call_boot_rom_wait);

    // set all other call backs & pointers...
    procIndication->set_riscy_htif(riscy_htif);
    procIndication->set_htif_args(&htif_args);
    perf_stats.set_req_perf(req_perf_counter);
    hostDmaIndication->set_req_proxy(hostDmaRequestProxy);
    // start mtohost handler thread
    procIndication->spawn_to_host_handler();

    // run tests
    int result = runHtifTest();

    //delete riscy_htif;
    //riscy_htif = NULL;

#ifdef SIMULATION
    unlink(socket_name);
#endif
    fflush(stdout);
    fclose(debug_file);
    return result;
}
