
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

// This was modified from htif.h in spike
#pragma once

#include <functional>
#include <mutex>
#include "fesvr/device.h"
#include "custom_memif.h"

// This HTIF handler class only handles exit code and terminal IO (bcd)

class htif_riscy_t
{
public:
    // function to write fromhost to FPGA
    typedef std::function<void(reg_t)> WriteFromHostFunc;

    htif_riscy_t(WriteFromHostFunc f);
    ~htif_riscy_t() {}

    // called whenever a message is sent to host
    void get_to_host(reg_t x);

    // console (bcd) stdin
    bool bcd_wait_for_stdin() { return bcd.waiting_for_stdin(); }
    void bcd_feed_stdin(int ch) { bcd.feed_stdin(ch); }

    // exit status
    bool has_exited() { return exited; }
    int get_exit_code() { return exit_code; }

    // locking
    void lock() { htif_lock.lock(); }
    void unlock() { htif_lock.unlock(); }

private:
    // verbosity in debug outputs
    const bool verbose;

    // write fromhost
    WriteFromHostFunc write_from_host;

    // lock for multithread
    std::mutex htif_lock;

    // device ids
    static const uint8_t exit_device_id = 0;
    static const uint8_t bcd_device_id = 1;
    // exit device
    bool exited;
    int exit_code;
    void handle_exit_cmd(command_t cmd);
    // bcd device
    bcd_t bcd;
};

