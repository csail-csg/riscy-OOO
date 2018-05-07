#include "SimBase.h"
#include "SimBW.h"
#include <string.h>

uint64_t SimBase::sim_cycle = 0;

SimBase* SimBase::create(int argc, char **argv,
                         std::vector<std::string> &htif_args) {
    if(argc < 2) {
        fprintf(stderr, "Usage: %s SIM_TYPE SIM_ARGS -- HTIF_ARGS\n", argv[0]);
        return NULL;
    }

    char *type = argv[1];
    if(strcmp(type, "BW") == 0) {
        return SimBW::create(argc, argv, htif_args);
    }
    fprintf(stderr, "Unknown SIM_TYPE %s\n", type);
    return NULL;
}
