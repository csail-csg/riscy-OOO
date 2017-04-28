
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

#include "host_dma.h"

HostDmaIndication::HostDmaIndication(int id) :
    HostDmaIndicationWrapper(id),
    verbose(false),
    // req proxy
    reqProxy(NULL),
    // pending req params
    read_pending(false),
    read_dst(NULL),
    read_total_burst_num(0),
    read_resp_cnt(0),
    first_read_skip_bytes(0),
    last_read_drop_bytes(0),
    write_pending(false)
{
}

HostDmaIndication::~HostDmaIndication() {}

void HostDmaIndication::rdData (const uint64_t data, const uint8_t id) {
    // grab lock and process the FPGA resp
    std::unique_lock<std::mutex> lock(process_mu);

    // check pending bit
    if(!read_pending) {
        fprintf(stderr, "[ERROR] HostDmaIndication gets rdData when there is no pending read\n");
        exit(1);
    }

    //if(verbose) {
    //    fprintf(stderr, "[HostDmaIndication] rdData(%llx, %d) burst %d\n",
    //            (long long unsigned)data, last, read_resp_cnt);
    //}
    
    char *buff = (char*)(&data);
    int burst_id = int(id);

    // get bytes to read and skip within this data
    int read_bytes = data_bytes;
    int skip_lsb = 0;
    if(burst_id == 0) {
        read_bytes -= first_read_skip_bytes;
        skip_lsb += first_read_skip_bytes;
    }
    if(burst_id == read_total_burst_num - 1) {
        read_bytes -= last_read_drop_bytes;
    }
    
    // get starting offset w.r.t read_dst to copy data
    int start_offset = burst_id * data_bytes;
    if(burst_id > 0) {
        start_offset -= first_read_skip_bytes;
    }
    // copy to buffer
    memcpy(read_dst + start_offset, buff + skip_lsb, read_bytes);

    // incr resp cnt
    read_resp_cnt++;

    // after getting all resp, notify done & reset pending bit
    if(read_resp_cnt == read_total_burst_num) {
        read_pending = false;
        process_cv.notify_one();
    }
}

void HostDmaIndication::wrDone () {
    // grab lock, check pending bit
    std::unique_lock<std::mutex> lock(process_mu);
    if(!write_pending) {
        fprintf(stderr, "[ERROR] HostDmaIndication gets wrDone when there is no pending write\n");
        exit(1);
    }
    // reset pending bit & notify done
    write_pending = false;
    process_cv.notify_one();
}

void HostDmaIndication::dma_read(addr_t addr, size_t len, void *dst) {
    // lock to ensure single in-flight dma req
    std::lock_guard<std::mutex> dma_lock(dma_mu);

    {
        // grab lock to set up parameters & req to FPGA
        std::unique_lock<std::mutex> process_lock(process_mu);
        if(read_pending) {
            fprintf(stderr, "[ERROR] HostDmaIndication gets dma_read when there is pending read\n");
            exit(1);
        }
        read_pending = true; // set read pending

        // set up params (for processing FPGA resp)
        read_dst = (char*)dst;
        read_resp_cnt = 0;
        first_read_skip_bytes = addr % data_bytes;
        last_read_drop_bytes = data_bytes - (addr + len) % data_bytes;
        if(last_read_drop_bytes == data_bytes) {
            last_read_drop_bytes = 0;
        }
        read_total_burst_num = (len + first_read_skip_bytes + last_read_drop_bytes) / data_bytes;

        // send req to FPGA
        addr_t start_addr = addr - first_read_skip_bytes;
        if(verbose) {
            fprintf(stderr, "[HostDmaIndication] dma_read(%llx, %d), "
                    "first skip %d, last drop %d, start addr %llx, bursts %d\n",
                    (long long)addr, (int)len, first_read_skip_bytes,
                    last_read_drop_bytes, (long long)start_addr, read_total_burst_num);
        }
        if(read_total_burst_num > max_burst_num) {
            fprintf(stderr, "[ERROR] dma_read needs %d bursts > max (%d)\n",
                    read_total_burst_num, max_burst_num);
            exit(1);
        }
        reqProxy->req(HostDmaRead, start_addr, read_total_burst_num - 1);

        // wait for resp
        process_cv.wait(process_lock, [&]{ return !read_pending; });
        if(verbose) {
            fprintf(stderr, "[HostDmaIndication] dma_read(%llx, %d) done\n",
                    (long long)addr, (int)len);
        }
    }
}

void HostDmaIndication::dma_write(addr_t addr, size_t len, const void *src) {
    // lock to ensure single in-flight dma req
    std::lock_guard<std::mutex> dma_lock(dma_mu);

    {
        // grab lock to req to FPGA
        std::unique_lock<std::mutex> process_lock(process_mu);
        if(write_pending) {
            fprintf(stderr, "[ERROR] HostDmaIndication gets dma_write when there is pending write\n");
            exit(1);
        }
        write_pending = true; // set write pending

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
            fprintf(stderr, "[HostDmaIndication] dma_write(%llx, %d), start addr %llx, bursts %d\n",
                    (long long)addr, (int)len, (long long)start_addr, burst_num);
        }
        if(burst_num > max_burst_num) {
            fprintf(stderr, "[ERROR] dma_write needs %d bursts > max (%d)\n",
                    burst_num, max_burst_num);
            exit(1);
        }

        // send first burst
        memcpy(buff + skip_lsb, write_src, write_bytes);
        uint8_t byte_en = get_write_be(skip_lsb, write_bytes);
        //if(verbose) {
        //    fprintf(stdout, "[HostDmaIndication] dma_write(%llx, %d) burst 0: write bytes %d, be %x\n",
        //            (long long)addr, (int)len, write_bytes, (unsigned)byte_en);
        //}
        reqProxy->req(HostDmaWrite, start_addr, burst_num - 1);
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
            //if(verbose) {
            //    fprintf(stdout, "[HostDmaIndication] dma_write(%llx, %d) burst %d: write bytes %d, be %x\n",
            //            (long long)addr, (int)len, i, write_bytes, (unsigned)byte_en);
            //}
        }

        // wait for resp
        process_cv.wait(process_lock, [&]{ return !write_pending; });
        if(verbose) {
            fprintf(stderr, "[HostDmaIndication] dma_write(%llx, %d) done\n",
                    (long long)addr, (int)len);
        }
    }
}
