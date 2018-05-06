#pragma once

#include "spike/sync_buffer.h"
#include "spike/decode.h"

class SimpleSim {
public:
    SimpleSim(sync_buffer_t<traced_inst_t> *trace);
    ~SimpleSim() {}
    void run();

private:
    sync_buffer_t<traced_inst_t> *inst_trace;
};
