#include "SimpleSim.h"
#include <stdio.h>
#include <time.h>
#include <algorithm>

SimpleSim::SimpleSim(sync_buffer_t<traced_inst_t> *trace, int load_to_use_lat) :
    inst_trace(trace), load_latency(load_to_use_lat)
{
    for(int i = 0; i < reg_num; i++) {
        reg_ready_time[i] = 0;
    }
}

void SimpleSim::run() {
    // stats
    bool stats = false;
    time_t start_time = 0, end_time = 0;
    uint64_t inst_count = 0;
    uint64_t load_count = 0;
    uint64_t cycle = 0;

    while(true) {
        // get trace: the trace memory will not be overwritten
        // before the next call of consume
        traced_inst_t &cur_inst = inst_trace->consume();

        if(unlikely(cur_inst.type != traced_inst_t::Inst)) {
            // special insts
            switch(cur_inst.type) {
                case traced_inst_t::Terminate:
                    fprintf(stderr, "[SimpleSim] done, "
                            "load to use latency = %d, sim time = %lld, "
                            "inst = %llu, cycle = %llu, load = %llu\n",
                            load_latency, (long long)(end_time - start_time),
                            (long long unsigned)inst_count, (long long unsigned)cycle,
                            (long long unsigned)load_count);
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
        } else if(stats) {
            // performance modelling
            inst_count++;
            // update reg ready time (FIXME ignore rs3 here...)
            uint8_t rd = cur_inst.rd;
            if(rd != 0) {
                bool load = is_load(cur_inst.inst);
                uint64_t latency = load ? load_latency : 1;
                uint64_t rd_time = std::max(reg_ready_time[cur_inst.rs1],
                                            reg_ready_time[cur_inst.rs2]) + latency;
                reg_ready_time[rd] = rd_time;
                cycle = std::max(cycle, rd_time);
                load_count += int(load);
            }
        }
    }
}
