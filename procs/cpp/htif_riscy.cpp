
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

#include <iostream>
#include <sstream>
#include <string>
#include <unistd.h>
#include <stdexcept>
#include <stdlib.h>
#include <errno.h>
#include <assert.h>
#include <stddef.h>
#include <poll.h>
#include <sys/wait.h>
#include <sys/types.h>
#include "htif_riscy.h"

htif_riscy_t::htif_riscy_t(WriteFromHostFunc f) :
    verbose(false),
    write_from_host(f),
    exited(false),
    exit_code(0)
{
}

void htif_riscy_t::handle_exit_cmd(command_t cmd) {
    if(cmd.cmd() == 0 && (cmd.payload() & 0x01ULL)) {
        exit_code = int(cmd.payload() >> 1);
        exited = true;
        if(verbose) {
            fprintf(stderr,"[htif riscy] exit payload %016llx\n",
                    (long long unsigned)(cmd.payload()));
        }
    }
    else {
        fprintf(stderr, "[htif riscy] ERROR: "
                "unknown exit cmd %d, payload %016llx\n",
                int(cmd.cmd()), (long long unsigned)(cmd.payload()));
        exit(1);
    }
}

void htif_riscy_t::get_to_host(reg_t x) {
    // The processor just wrote x to the to_host register
    if(verbose) {
        fprintf(stderr, "[htif riscy] get_to_host(0x%016llx)\n",
                (long long unsigned)x);
    }

    command_t cmd(nullptr, x, write_from_host);

    int dev = int(cmd.device());
    switch(dev) {
        case exit_device_id:
            handle_exit_cmd(cmd);
            break;
        case bcd_device_id:
            bcd.handle_command(cmd);
            break;
        default:
            fprintf(stderr, "[htif riscy] ERROR: unknown dev %d\n", int(dev));
            exit(1);
            break;
    }
}

