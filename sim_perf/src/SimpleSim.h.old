#pragma once

#include "spike/sync_buffer.h"
#include "spike/decode.h"

class SimpleSim {
public:
    SimpleSim(sync_buffer_t<traced_inst_t> *trace, int load_to_use_lat);
    ~SimpleSim() {}
    void run();

private:
    sync_buffer_t<traced_inst_t> *inst_trace;

    // load to use latency
    const int load_latency;

    // reg ready time
    static const int reg_num = 64; // 32 int + 32 fp
    uint64_t reg_ready_time[reg_num];

    // check if inst is load
    bool is_load(uint32_t inst) {
        uint32_t opcode = (inst >> 2) & 0x01F;
        return opcode == 0 || opcode == 1; // Load/LoadFp
    }

};
