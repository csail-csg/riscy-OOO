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

    // create performance model
    SimBW *perf = new SimBW(load_latency);

    // create and run spike
    std::vector<std::string> htif_args;
    for(int i = 2; i < argc; i++) {
        htif_args.push_back(std::string(argv[i]));
    }
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
