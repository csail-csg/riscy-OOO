#include "SimpleSim.h"
#include <stdio.h>
#include <time.h>

SimpleSim::SimpleSim(sync_buffer_t<traced_inst_t> *trace) :
    inst_trace(trace)
{
}

void SimpleSim::run() {
    time_t start_time = 0, end_time = 0;
    bool stats = false;
    uint64_t inst_count = 0;
    while(true) {
        traced_inst_t inst = inst_trace->consume();
        if(stats) {
            inst_count++;
        }
        if(unlikely(inst.type != traced_inst_t::Inst)) {
            switch(inst.type) {
                case traced_inst_t::Terminate:
                    fprintf(stderr, "[SimpleSim] done, inst count = %llu, time = %lld\n",
                            (long long unsigned)inst_count, (long long)(end_time - start_time));
                    return;
                case traced_inst_t::BeginStats:
                    start_time = time(0);
                    stats = true;
                    break;
                case traced_inst_t::EndStats:
                    end_time = time(0);
                    stats = false;
                    break;
            }
        }
    }
}
