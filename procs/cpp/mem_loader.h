
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
#include <stdlib.h>
#include <string.h>
#include <mutex>
#include <thread>
#include <condition_variable>
#include <sys/types.h>
#include "GeneratedTypes.h"
#include "MemLoaderIndication.h"
#include "MemLoaderRequest.h"
#include "custom_memif.h"
#include "fesvr/elfloader.h" // FIXME must be after memif for reg_t definition

class MemLoaderIndication;

// memif passed to load_elf() to load elf file to FPGA
class mem_loader_memif_t : public memif_t {
private:
    const bool verbose;
    // req proxy to FPGA
    MemLoaderRequestProxy *reqProxy;
    // indication
    MemLoaderIndication *indication;
    // number of bytes in one write data (burst) to FPGA
    static const int data_bytes = sizeof(uint64_t);

    // helper to get write byte enable
    uint8_t inline get_write_be(int skip_lsb, int write_bytes) {
        uint8_t be = (uint8_t)(-1);
        be = be >> (data_bytes - write_bytes);
        be = be << skip_lsb;
        return be;
    }

public:
    mem_loader_memif_t(MemLoaderRequestProxy *req, MemLoaderIndication *ind);
    virtual ~mem_loader_memif_t() {}

    // loading elf to mem doesn't need read
    virtual void read(addr_t addr, size_t len, void* bytes) {
        fprintf(stderr, "[mem loader] ERROR: should not read\n");
        exit(1);
    }

    // function to write to FPGA
    virtual void write(addr_t addr, size_t len, const void* bytes);
};


class MemLoaderIndication : public MemLoaderIndicationWrapper {
private:
    // sem signaled by FPGA when it finishes a series of writes
    sem_t write_done_sem;

    // req proxy to FPGA
    MemLoaderRequestProxy *reqProxy;

    // elf file to be loaded
    const char *elf_file;
    reg_t elf_entry_addr;

    // we load elf to FPGA in a new thread
    std::thread load_elf_thread;
    void load_elf_handler();

public:
    MemLoaderIndication(int id, const char *elf_file);
    virtual ~MemLoaderIndication();

    virtual void start(uint64_t mem_addr);
    virtual void wrDone();

    void waitWriteDone() { sem_wait(&write_done_sem); }

    void set_req_proxy(MemLoaderRequestProxy *p) {
        reqProxy = p;
        // keep retrying if fail to send request
        reqProxy->pint.busyType = BUSY_SPIN;
    }
};
