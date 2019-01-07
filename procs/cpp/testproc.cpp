
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
#include "boot_rom.h"
#include "mem_loader.h"
#include "mmio.h"
#include "deadlock.h"
#include "rename_debug.h"

#ifdef NDEBUG
#error fesvr will not work with NDEBUG defined
#endif

#define BLURT fprintf (stderr, "CPPDEBUG: %s(%s):%d\n",\
                      __func__, __FILE__, __LINE__)

// File for output
static FILE *debug_file = 0;
static PrintBuffer *print_buff = 0;
FILE *perf_fp = stderr; // performance counters output file

// proc requests & indications
static ProcRequestProxy *procRequestProxy = 0;
static ProcIndication   *procIndication = 0;
static CorePerfStats core_perf_stats;
static UncorePerfStats uncore_perf_stats;

// mem loader request & indications
static MemLoaderRequestProxy *memLoaderRequestProxy = 0;
static MemLoaderIndication *memLoaderIndication = 0;

// boot rom request
static BootRomRequestProxy *bootRomRequestProxy = 0;
static BootRomIndication *bootRomIndication = 0;

// deadlock detect
static DeadlockIndication *deadlockIndication = 0;
static long long unsigned ignore_user_commit_stucks = 0;

// rename debug
static RenameDebugIndication *renameDebugIndication = 0;

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
static void req_perf_counter(uint8_t core, PerfLocation loc, PerfType type) {
    procRequestProxy->perfReq(core, loc, type);
}

static void get_htif_addrs(const char *elf_file,
                           uint64_t &tohost, uint64_t &fromhost) {
    dummy_memif_t mif;
    uint64_t entry;
    std::map<std::string, uint64_t> symbols = load_elf(elf_file, &mif, &entry);
    if(symbols.count("tohost") && symbols.count("fromhost")) {
        tohost = symbols["tohost"];
        fromhost = symbols["fromhost"];
    } else {
        fprintf(stderr, "ERROR: "
                "fail to find tohost/fromhost in elf %s\n", elf_file);
        exit(1);
    }
}

int runTest(const char *elf_file, int core_num, uint64_t mem_size) {
    sleep(1);

    // program boot rom
    fprintf(stderr, "programming boot rom\n");
    bootRomIndication->init_boot_rom(bootRomRequestProxy);
    sleep(1);

    // set deadlock start inst num
    fprintf(stderr, "deadlock check starts with processor, "
            "but ignores first %llu user commit stucks\n",
            ignore_user_commit_stucks);

    // figure out addr overflow mask
    int log_mem_sz = 0;
    for(log_mem_sz = 0; log_mem_sz < 64; log_mem_sz++) {
        if(mem_size == (uint64_t(1) << log_mem_sz)) {
            break;
        }
    }
    if(log_mem_sz >= 64) {
        fprintf(stderr, "memory size %llu B is not power of 2\n",
                (long long unsigned)mem_size);
        exit(-1);
    }
    uint64_t addr_overflow_mask = (~uint64_t(0)) << log_mem_sz;

    // start processor
    uint64_t startpc = BOOT_ROM_BASE;
    uint64_t tohost_addr = 0;
    uint64_t fromhost_addr = 0;
    get_htif_addrs(elf_file, tohost_addr, fromhost_addr);
    fprintf(stderr, "startpc %llx, total %d cores, "
            "toHost addr %llx, fromHost addr %llx, "
            "addr overflow mask %016llx\n",
            (long long unsigned)startpc, (int)core_num,
            (long long unsigned)tohost_addr,
            (long long unsigned)fromhost_addr,
            (long long unsigned)addr_overflow_mask);
    procRequestProxy->start(startpc, tohost_addr, fromhost_addr,
                            addr_overflow_mask);

    // wait for result
    int result = procIndication->waitResult();

    // performance result
    for(uint32_t i = 0; i < (uint32_t)core_num; i++) {
        core_perf_stats.reset(); // clear old data
        fprintf(perf_fp, "getting performance results for core %d\n", i);
        core_perf_stats.get_all_perf(i);
        core_perf_stats.print(perf_fp);
        fprintf(perf_fp, "\n");
    }
    uncore_perf_stats.reset();
    fprintf(perf_fp, "getting performance results for uncore\n");
    uncore_perf_stats.get_all_perf(core_num); // uncore id = core num
    uncore_perf_stats.print(perf_fp);
    if(perf_fp != stderr && perf_fp != stdout) {
        fclose(perf_fp);
    }

    return result;
}

void printHelp(const char *prog) {
    fprintf(stderr, "Usage: %s [--assembly-tests] ", prog);
    fprintf(stderr, "[--mem-size MEM_MB_SIZE] ");
    fprintf(stderr, "[--ignore-user-stucks IGNORED_USER_COMMIT_STUCK_NUM] ");
    fprintf(stderr, "[--core-num CORE_NUM] ");
    fprintf(stderr, "[--shell-cmd CMD DELAY(sec)] ");
    fprintf(stderr, "[--perf-file PERF_OUTPUT_FILE] ");
    fprintf(stderr, "[--rom ROM_BINARY] ");
    fprintf(stderr, "[--elf ELF_BINARY]\n");
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
    const char *elf_file = 0;
    const char *rom_file = 0;
    uint64_t mem_size = 64*1024*1024; // default 64 MB memory
    uint32_t core_num = 1; // default to 1 core

    while(1) {
        if (argc > 1 && strcmp(argv[1],"--assembly-tests") == 0) {
            // first argument was "--assembly-tests"
            // this uses an alternate handler for tohost messages ...
            assembly_test_mode = true;
            // shift argc and argv accordingly
            argc--; argv++;
        } else if (argc > 2 && strcmp(argv[1],"--mem-size") == 0) {
            // first argument was "--mem-size"
            // second argument should be memory size in MB
            mem_size = (size_t)(atoi(argv[2])) * 1024 * 1024;
            // shift argc and argv accordingly
            argc-=2; argv+=2;
        } else if(argc > 2 && strcmp(argv[1],"--ignore-user-stucks") == 0) {
            // first argument was "--ignore-user-stucks"
            // second argument should be number of stucks to ignore
            ignore_user_commit_stucks = std::stoull(argv[2]);
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
        } else if(argc > 2 && strcmp(argv[1], "--rom") == 0) {
            // --rom BOOT_ROM_BINARY
            rom_file = argv[2];
            argc-=2; argv+=2;
        } else if(argc > 2 && strcmp(argv[1], "--elf") == 0) {
            // --elf ELF_BINARY
            elf_file = argv[2];
            argc-=2; argv+=2;
        } else if(argc == 1) {
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
                                                    debug_file,
                                                    print_buff,
                                                    core_num,
                                                    &core_perf_stats,
                                                    &uncore_perf_stats,
                                                    &handle_signal,
                                                    shell_cmd,
                                                    cmd_delay_sec);
    } else {
        procIndication = new ProcIndication(IfcNames_ProcIndicationH2S,
                                            debug_file,
                                            print_buff,
                                            core_num,
                                            &core_perf_stats,
                                            &uncore_perf_stats,
                                            &handle_signal,
                                            shell_cmd,
                                            cmd_delay_sec);
    }
    procRequestProxy = new ProcRequestProxy(IfcNames_ProcRequestS2H);

    bootRomIndication = new BootRomIndication(IfcNames_BootRomIndicationH2S,
                                              rom_file, core_num, mem_size);
    bootRomRequestProxy = new BootRomRequestProxy(IfcNames_BootRomRequestS2H);

    memLoaderIndication = new MemLoaderIndication(IfcNames_MemLoaderIndicationH2S,
                                                  elf_file);
    memLoaderRequestProxy = new MemLoaderRequestProxy(IfcNames_MemLoaderRequestS2H);

    deadlockIndication = new DeadlockIndication(IfcNames_DeadlockIndicationH2S,
                                                ignore_user_commit_stucks);

    renameDebugIndication = new RenameDebugIndication(IfcNames_RenameDebugIndicationH2S);

    // create htif
    htif_riscy_t *riscy_htif = new htif_riscy_t(call_from_host);

    // set all other call backs & pointers...
    procIndication->set_riscy_htif(riscy_htif);
    core_perf_stats.set_req_perf(req_perf_counter);
    uncore_perf_stats.set_req_perf(req_perf_counter);
    memLoaderIndication->set_req_proxy(memLoaderRequestProxy);
    // start mtohost handler thread
    procIndication->spawn_to_host_handler();

    // run tests
    int result = runTest(elf_file, core_num, mem_size);

    //delete riscy_htif;
    //riscy_htif = NULL;

#ifdef SIMULATION
    unlink(socket_name);
#endif
    fflush(stdout);
    fclose(debug_file);
    return result;
}
