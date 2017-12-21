
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

// See LICENSE for license details.
// This was modified from htif.cc in spike
#include <unistd.h>
#include <stdexcept>
#include <stdlib.h>
#include <errno.h>
#include <assert.h>
#include <stddef.h>
#include <poll.h>
#include "htif_riscy.h"
#include "spike/encoding.h"

htif_riscy_t::htif_riscy_t(const std::vector<std::string>& args, uint32_t _core_num) :
    htif_t(args),
    core_num(_core_num),
    memsz(0),
    dma_read(nullptr),
    dma_write(nullptr),
    write_from_host(nullptr),
    verbose(false)
{
}

void htif_riscy_t::get_to_host(uint32_t coreid, reg_t x)
{
    // The processor just wrote x to the to_host register
    // Hopefully things are initialized
    assert(memsz > 0);

    if (verbose) fprintf(stderr, "htif_riscy_t::get_to_host(0x%llx)\n", (long long)x);

    command_t cmd(this, x, std::bind(write_from_host, coreid, std::placeholders::_1));
    device_list.handle_command(cmd);

    if (exitcode != 0) {
        stop();
    }
}

reg_t htif_riscy_t::read_cr(uint32_t coreid, uint16_t regnum)
{
    if (verbose) fprintf(stderr, "htif_riscy_t::read_cr(%d, %d)\n", coreid, regnum);
    if ((coreid & 0xFFFFF) == 0xFFFFF) // system control register space
    {
        switch (regnum) {
            case 0:     return core_num; // number of procs
            case 1:     return memsz >> 20; // size of memory in MB
            default:    return -1;
        }
    } else {
        switch (regnum)
        {
            case CSR_MTOHOST:
                // reading mtohost
                if (verbose) fprintf(stderr, ">> ERROR mtohost shouldn't be read this way\n");
                return 0;
            case CSR_MFROMHOST:
                return 0;
            case CSR_MRESET:
                // reading reset csr
                if (verbose) fprintf(stderr, ">> Reading mreset CSR\n");
                return 0;
            default:
                if (verbose) fprintf(stderr, ">> ERROR Reading unknown CSR\n");
                return 0;
        }
    }

    if (verbose) fprintf(stderr, ">> ERROR didn't find CSR to read\n");
    return -1;
}

reg_t htif_riscy_t::write_cr(uint32_t coreid, uint16_t regnum, reg_t val)
{
    if (verbose) fprintf(stderr, "htif_riscy_t::write_cr(%d, %d, %lld)\n", coreid, regnum, (long long)val);
    if ((coreid & 0xFFFFF) == 0xFFFFF) // system control register space
    {
        // no action for writing these
        if (1) fprintf(stderr, ">> WARNING writing to read-only registers\n");
        switch (regnum) {
            case 0:     return core_num; // number of procs
            case 1:     return memsz >> 20; // size of memory in MB
            default:    return -1;
        }
    } else {
        switch (regnum)
        {
            case CSR_MTOHOST:
                // reading mtohost
                if (1) fprintf(stderr, ">> WARNING writing to mtohost\n");
                return 0;
            case CSR_MFROMHOST:
                write_from_host(coreid, val);
                return 0;
            case CSR_MRESET:
                // reading reset csr
                // [sizhuo] make it a warnining (spike doesn't rely on MRESET now)
                if (1) fprintf(stderr, ">> WARNING writing to mreset not implemented yet\n");
                return 0;
            default:
                if (1) fprintf(stderr, ">> ERROR writing to unknown CSR\n");
                return 0;
        }
    }

    if (verbose) fprintf(stderr, ">> ERROR didn't find CSR to write to\n");
    return -1;
}

void htif_riscy_t::read_chunk(addr_t taddr, size_t len, void* dst)
{
    assert(taddr >= 0);
    assert(taddr + len <= memsz);

    if (verbose) fprintf(stderr, "htif_riscy_t::read_chunk(taddr=0x%lx, len=%ld, dst=%p)\n", (long)taddr, (long)len, dst);

    //if (verbose) fprintf(stderr, ">> destination 0x%p\n", &membuff[taddr/sizeof(uint64_t)]);
    dma_read(taddr, len, dst);
}

void htif_riscy_t::write_chunk(addr_t taddr, size_t len, const void* src)
{
    assert(taddr >= 0);
    assert(taddr + len <= memsz);

    if (verbose) fprintf(stderr, "htif_riscy_t::write_chunk(taddr=0x%lx, len=%ld, src=%p)\n", (long)taddr, (long)len, src);

    //if (verbose) fprintf(stderr, ">> destination 0x%p\n", &membuff[taddr/sizeof(uint64_t)]);
    dma_write(taddr, len, src);
}

bool htif_riscy_t::bcd_wait_for_stdin() {
    device_t *bcd = device_list.get_bcd();
    if(bcd) {
        return bcd->wait_for_stdin();
    } else {
        return false;
    }
}

void htif_riscy_t::bcd_feed_stdin(int ch) 
{
    device_t *bcd = device_list.get_bcd();
    if(bcd) {
        bcd->feed_stdin(ch);
    } else {
        fprintf(stderr, ">> ERROR: there is no device bcd\n");
    }
}
