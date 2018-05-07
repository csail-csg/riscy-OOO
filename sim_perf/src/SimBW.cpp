#include "SimBW.h"

SimBW::SimBW(int lat_load_to_use) :
    latency_load_to_use(lat_load_to_use),
    rob(rob_size + 1),
    rob_enq_idx(0),
    rob_deq_idx(0),
    dispatch_mem_port(dispatch_mem_bw),
    dispatch_alu_port(dispatch_alu_bw),
    writeback_port(writeback_bw),
    inst_count(0),
    load_count(0)
{
    for(int i = 0; i < reg_num; i++) {
        rt_busy[i] = NULL;
        rt_ready[i] = 0;
    }
}

void SimBW::run() {
    fast_forward();
    sim();
    dump_stats();
}

void SimBW::fast_forward() {
    while(true) {
        traced_inst_t &inst = inst_trace.consume();
        if(unlikely(inst.begin_stats)) {
            fprintf(stderr, "[SimBW] Fastforward done, begin simulation\n");
            return;
        }
    }
}

void SimBW::dump_stats() {
    fprintf(stderr, "[SimBW] Simulation done. "
            "inst count %llu, cycle %llu, load count %llu\n",
            (long long unsigned)inst_count, (long long unsigned)sim_cycle,
            (long long unsigned)load_count);
}

void SimBW::sim() {
    while(true) {
        bool end_sim = commit();
        if(unlikely(end_sim)) {
            return;
        }
        writeback_port.process();
        dispatch_mem_port.process();
        dispatch_alu_port.process();
        rename();
        // advance clock
        sim_cycle++;
    }
}

bool SimBW::commit() {
    for(int i = 0; i < commit_bw && !rob_empty(); i++) {
        if(rob[rob_deq_idx].executed) {
            if(unlikely(rob[rob_deq_idx].end_sim)) {
                return true;
            }
            rob_deq_idx = incr_rob_idx(rob_deq_idx);
        } else {
            break;
        }
    }
    return false;
}

void SimBW::rename() {
    for(int i = 0; i < commit_bw && !rob_full(); i++) {
        traced_inst_t &trace = inst_trace.consume();
        RenamedInst *inst = &(rob[rob_enq_idx]);
        rob_enq_idx = incr_rob_idx(rob_enq_idx);
        rename_inst(trace, inst);
    }
}

void SimBW::rename_inst(const traced_inst_t &trace, RenamedInst *inst) {
    // figure out src dependency on older inst
    inst->src_ready_time = sim_cycle;
    inst->src_pend_num = 0;
    for(int i = 0; i < RenamedInst::src_num; i++) {
        uint8_t src_reg = trace.rs[i];
        RenamedInst *old_inst = rt_busy[src_reg];
        if(old_inst) {
            // src operand is not ready
            inst->src_pend_num++;
            // add to old inst's dependent list
            RenamedInst::SrcPendNode *pend = &(inst->src_pend[i]);
            if(old_inst->first_dep) {
                old_inst->last_dep->next = pend;
            } else {
                old_inst->first_dep = pend;
            }
            pend->next = NULL;
            old_inst->last_dep = pend;
        } else {
            inst->src_ready_time = std::max(inst->src_ready_time, rt_ready[src_reg]);
        }
    }

    // set dst in rename table to catch future dependent insts
    uint8_t dst_reg = trace.rd;
    inst->dst_reg = dst_reg;
    inst->first_dep = NULL;
    inst->last_dep = NULL;
    if(dst_reg) {
        rt_busy[dst_reg] = inst;
    }

    // set up dispatch/latency info
    bool load = false, store = false;
    is_mem(trace.inst, load, store);
    inst->dispatch_port = (load || store) ? &dispatch_mem_port : &dispatch_alu_port;
    if(load) {
        inst->latency_to_use = latency_load_to_use;
        inst->latency_to_wb = latency_load_to_wb;
    } else {
        inst->latency_to_use = latency_non_load_to_use;
        inst->latency_to_wb = latency_non_load_to_wb;
    }

    // not yet executed
    inst->executed = false;

    // set end sim
    inst->end_sim = trace.end_stats;

    // dispatch the inst if all src ready
    if(inst->src_pend_num == 0) {
        schedule_dispatch(inst, inst->src_ready_time);
    }

    // stats
    inst_count++;
    load_count += uint64_t(load);
}

void SimBW::dispatch(RenamedInst *inst) {
    // wake up younger inst
    uint64_t ready_to_use_time = sim_cycle + inst->latency_to_use;
    for(RenamedInst::SrcPendNode *pend_src = inst->first_dep;
        pend_src != NULL; pend_src = pend_src->next) {
        RenamedInst *young_inst = pend_src->self;
        young_inst->src_pend_num--;
        young_inst->src_ready_time = std::max(young_inst->src_ready_time, ready_to_use_time);
        // schedule younger inst to dispatch if all src ready
        if(young_inst->src_pend_num == 0) {
            schedule_dispatch(young_inst, young_inst->src_ready_time);
        }
    }

    // clear rename table & set ready time
    uint8_t dst_reg = inst->dst_reg;
    if(rt_busy[dst_reg] == inst) {
        rt_busy[dst_reg] = NULL;
        rt_ready[dst_reg] = ready_to_use_time;
    }

    // schedule write back
    schedule_writeback(inst, sim_cycle + inst->latency_to_wb);
}

void SimBW::writeback(RenamedInst *inst) {
    inst->executed = true;
}
