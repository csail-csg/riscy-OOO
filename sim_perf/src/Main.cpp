#include <pthread.h>
#include <vector>
#include <string>
#include "spike/sim.h"
#include "SimBW.h"

void *run_spike(void *spike) {
    ((sim_t*)spike)->run();
    return nullptr;
}

int main(int argc, char *argv[]) {

    if(argc < 3) {
        fprintf(stderr, "Usage: %s LOAD_LATENCY HTIF_ARG1 HTIF_ARG2 ...\n", argv[0]);
        return 0;
    }
    int load_latency = atoi(argv[1]);

    // parse args & create performance model
    std::vector<std::string> htif_args;
    SimBase *perf = SimBase::create(argc, argv, htif_args);
    if(!perf) {
        return -1;
    }

    // create and run spike
    std::vector<std::pair<reg_t, mem_t*>> mems;
    mems.push_back(std::make_pair(reg_t(DRAM_BASE), new mem_t(reg_t(2048) << 20)));
    sync_buffer_t<traced_inst_t>* trace[] = { &(perf->inst_trace) };
    sim_t *spike = new sim_t("RV64IMAFD", 1, false, reg_t(-1), mems, htif_args, trace);
    pthread_t spike_tid;
    pthread_create(&spike_tid, NULL, run_spike, spike);

    // run performance model
    perf->run();

    return 0;
}
