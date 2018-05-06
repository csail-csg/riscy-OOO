#include <pthread.h>
#include <vector>
#include <string>
#include "spike/sim.h"
#include "SimpleSim.h"

void *run_spike(void *spike) {
    ((sim_t*)spike)->run();
    return nullptr;
}

int main(int argc, char *argv[]) {
    // inst trace buffer
    sync_buffer_t<traced_inst_t> *inst_trace = new sync_buffer_t<traced_inst_t>(1024 * 1024);

    // run spike
    std::vector<std::string> htif_args;
    for(int i = 1; i < argc; i++) {
        htif_args.push_back(std::string(argv[i]));
    }
    std::vector<std::pair<reg_t, mem_t*>> mems;
    mems.push_back(std::make_pair(reg_t(DRAM_BASE), new mem_t(reg_t(2048) << 20)));
    sim_t *spike = new sim_t("RV64IMAFD", 1, false, reg_t(-1), mems, htif_args, &inst_trace);
    pthread_t spike_tid;
    pthread_create(&spike_tid, NULL, run_spike, spike);

    // run performance model
    SimpleSim *perf = new SimpleSim(inst_trace);
    perf->run();

    return 0;
}
