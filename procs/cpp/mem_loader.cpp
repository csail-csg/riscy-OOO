
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

#include "mem_loader.h"

mem_loader_memif_t::mem_loader_memif_t(MemLoaderRequestProxy *req,
                                       MemLoaderIndication *ind) :
    memif_t(nullptr), verbose(false), reqProxy(req), indication(ind)
{
}

void mem_loader_memif_t::write(addr_t addr, size_t len, const void *src) {
    fprintf(stderr, "[mem loader] write(%016llx, %d)\n",
            (long long unsigned)addr, (int)len);

    // check for 0 write length (it can really happen...)
    if(len == 0) return;

    const char *write_src = (const char*)src;
    int remain_bytes = len;

    uint64_t data = 0;
    char *buff = (char*)(&data);

    // setup for the first burst
    int skip_lsb = addr % data_bytes;
    addr_t start_addr = addr - skip_lsb;
    int write_bytes = std::min(data_bytes - skip_lsb, remain_bytes);
    int burst_num = (remain_bytes + skip_lsb) / data_bytes;
    if((remain_bytes + skip_lsb) % data_bytes) {
        burst_num++;
    }
    if(verbose) {
        fprintf(stderr, "[mem loader] write(%016llx, %d), "
                "start addr %016llx, bursts %d\n",
                (long long unsigned)addr, (int)len,
                (long long unsigned)start_addr, burst_num);
    }

    // send first burst
    memcpy(buff + skip_lsb, write_src, write_bytes);
    uint8_t byte_en = get_write_be(skip_lsb, write_bytes);
    if(verbose) {
        fprintf(stdout, "[mem loader] write(%016llx, %d) burst 0: "
                "write bytes %d, data %016llx, be %02x\n",
                (long long unsigned)addr, (int)len,
                write_bytes, (long long unsigned)data, (unsigned char)byte_en);
    }
    reqProxy->wrAddr(true, start_addr);
    reqProxy->wrData(data, byte_en, burst_num == 1);
    // change ptr & write
    write_src += write_bytes;
    remain_bytes -= write_bytes;

    // send remaining bursts
    for(int i = 1; i < burst_num; i++) {
        write_bytes = std::min(remain_bytes, data_bytes);
        data = 0;
        memcpy(buff, write_src, write_bytes);
        byte_en = get_write_be(0, write_bytes);
        reqProxy->wrData(data, byte_en, i == burst_num - 1);
        write_src += write_bytes;
        remain_bytes -= write_bytes;
        if(verbose) {
            fprintf(stdout, "[mem loader] write(%016llx, %d) burst %d: "
                    "write bytes %d, data %016llx, be %02x\n",
                    (long long unsigned)addr, (int)len, i,
                    write_bytes, (long long unsigned)data,
                    (unsigned char)byte_en);
        }
    }

    // wait for completion
    indication->waitWriteDone();
}

MemLoaderIndication::MemLoaderIndication(int id, const char *elf) :
    MemLoaderIndicationWrapper(id),
    reqProxy(NULL),
    elf_file(elf),
    elf_entry_addr(0)
{
    sem_init(&write_done_sem, 0, 0);
    // figure out entry point for elf
    dummy_memif_t mif;
    load_elf(elf_file, &mif, &elf_entry_addr);
}

MemLoaderIndication::~MemLoaderIndication() {
    load_elf_thread.join();
}

void MemLoaderIndication::load_elf_handler() {
    mem_loader_memif_t mif(reqProxy, this);
    load_elf(elf_file, &mif, &elf_entry_addr);
    // tell FPGA that all writes for loading elf to memory are done
    reqProxy->wrAddr(false, 0);
    fprintf(stderr, "[mem loader] finish loading elf\n");
}

void MemLoaderIndication::wrDone () {
    sem_post(&write_done_sem);
}

void MemLoaderIndication::start(uint64_t mem_addr) {
    fprintf(stderr, "[mem loader] start loading elf %s, start addr %016llx\n",
            elf_file, (long long unsigned)mem_addr);
    // check mem_addr match elf entry point
    if(mem_addr != elf_entry_addr) {
        fprintf(stderr, "[mem loader] ERROR: "
                "start(%016llx) != entry addr %016llx\n",
                (long long unsigned)mem_addr,
                (long long unsigned)elf_entry_addr);
        exit(1);
    }
    load_elf_thread = std::thread(&MemLoaderIndication::load_elf_handler,
                                  this);
}

