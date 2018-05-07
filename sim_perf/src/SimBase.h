#pragma once
#include "spike/sync_buffer.h"
#include "spike/decode.h"
#include <vector>
#include <string>

class SimBase {
public:
    SimBase() : inst_trace(1024 * 1024) {}
    virtual ~SimBase() {}
    virtual void run() = 0;

    sync_buffer_t<traced_inst_t> inst_trace;

    static uint64_t inline cur_cycle() { return sim_cycle; }

    static SimBase* create(int argc, char **argv, std::vector<std::string> &htif_args);

protected:
    static uint64_t sim_cycle;
};
