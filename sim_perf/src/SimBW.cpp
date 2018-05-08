#include "SimBW.h"
#include <unistd.h>
#include <string.h>

void SimBW::usage(char *prog) {
    fprintf(stderr, "Usage: %s BW --lat LOAD_TO_USE_LAT "
            "--out OUT_FILE [--ff FAST_FORWARD_INSTS] "
            "[--sim SIM_INSTS] -- HTIF_ARGS\n", prog);
}

SimBW* SimBW::create(int argc, char **argv,
                     std::vector<std::string> &htif_args) {
    int load_lat = 0;
    char *out_file = NULL;
    uint64_t forward_insts = 0;
    uint64_t sim_insts = uint64_t(-1);

    // start from argv[2]
    int i = 2;
    while(i < argc) {
        if(strcmp(argv[i], "--lat") == 0 && i + 1 < argc) {
            load_lat = atoi(argv[i + 1]);
            i += 2;
        } else if(strcmp(argv[i], "--out") == 0 && i + 1 < argc) {
            out_file = argv[i + 1];
            i += 2;
        } else if(strcmp(argv[i], "--ff") == 0 && i + 1 < argc) {
            forward_insts = std::stoull(argv[i + 1]);
            i += 2;
        } else if(strcmp(argv[i], "--sim") == 0 && i + 1 < argc) {
            sim_insts = std::stoull(argv[i + 1]);
            i += 2;
        } else if(strcmp(argv[i], "--") == 0) {
            i++;
            break;
        } else {
            usage(argv[0]);
            return NULL;
        }
    }
    if(load_lat <= 0 || out_file == NULL) {
        usage(argv[0]);
        return NULL;
    }

    for(; i < argc; i++) {
        htif_args.push_back(std::string(argv[i]));
    }

    return new SimBW(load_lat, forward_insts, sim_insts, out_file);
}

SimBW::SimBW(int lat_load_to_use, uint64_t forward_insts,
             uint64_t sim_insts, const char *file) :
    latency_load_to_use(lat_load_to_use),
    rob(rob_size + 1),
    rob_enq_idx(0),
    rob_deq_idx(0),
    dispatch_mem_port(dispatch_mem_bw),
    dispatch_alu_port(dispatch_alu_bw),
    writeback_port(writeback_bw),
    forward_inst_num(forward_insts),
    sim_inst_num(sim_insts),
    inst_count(0),
    load_count(0),
    out_file(file)
{
    for(int i = 0; i < reg_num; i++) {
        rt_busy[i] = NULL;
        rt_ready[i] = 0;
    }

    dispatch_mem_port.set_callback(SimBW::call_dispatch, this);
    dispatch_alu_port.set_callback(SimBW::call_dispatch, this);
    writeback_port.set_callback(SimBW::call_writeback, this);

    fprintf(stderr, "\n[SimBW] init: load to use latency %d, "
            "forward insts %llu, simulate insts %llu, "
            "output file %s\n", lat_load_to_use,
            (long long unsigned)forward_insts,
            (long long unsigned)sim_insts, file);
}

void SimBW::run() {
    fast_forward();

    real_time = time(0);
    sim();
    real_time = time(0) - real_time;

    dump_stats();
}

void SimBW::fast_forward() {
    while(true) {
        traced_inst_t &inst = inst_trace.consume();
        if(unlikely(inst.begin_stats)) {
            break;
        }
    }
    for(uint64_t i = 0; i < forward_inst_num; i++) {
        traced_inst_t &inst = inst_trace.consume();
        if(unlikely(inst.end_stats)) {
            fprintf(stderr, "\n[SimBW] hit end sim before forwarding is done, exiting ...\n");
            exit(0);
        }
    }
    fprintf(stderr, "\n[SimBW] Fastforward done, begin simulation\n");
}

void SimBW::dump_stats() {
    sleep(1);
    fprintf(stderr, "\n[SimBW] Simulation done\n");
    FILE *fp = fopen(out_file.c_str(), "w");
    fprintf(fp, "load to use latency = %d\n", latency_load_to_use);
    fprintf(fp, "forwarded insts = %llu\n", (long long unsigned)forward_inst_num);
    fprintf(fp, "simulated insts = %llu\n", (long long unsigned)inst_count);
    fprintf(fp, "cycles = %llu\n", (long long unsigned)sim_cycle);
    fprintf(fp, "load insts = %llu\n", (long long unsigned)load_count);
    fprintf(fp, "real time seconds = %llu\n", (long long unsigned)real_time);
    fclose(fp);
}

void SimBW::sim() {
    while(inst_count < sim_inst_num) {
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
        schedule_dispatch(inst->src_ready_time, inst);
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
            schedule_dispatch(young_inst->src_ready_time, young_inst);
        }
    }

    // clear rename table & set ready time
    uint8_t dst_reg = inst->dst_reg;
    if(rt_busy[dst_reg] == inst) {
        rt_busy[dst_reg] = NULL;
        rt_ready[dst_reg] = ready_to_use_time;
    }

    // schedule write back
    schedule_writeback(sim_cycle + inst->latency_to_wb, inst);
}

void SimBW::writeback(RenamedInst *inst) {
    inst->executed = true;
}
