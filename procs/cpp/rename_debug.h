#pragma once
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include "GeneratedTypes.h"
#include "RenameDebugIndication.h"

class RenameDebugIndication : public RenameDebugIndicationWrapper {
private:
    FILE *log_fp;

public:
    RenameDebugIndication(int id) : RenameDebugIndicationWrapper(id), log_fp(0) {
        log_fp = fopen("rename_debug.txt", "w");
    }
    
    virtual ~RenameDebugIndication() {
        if(log_fp) {
            fclose(log_fp);
            log_fp = 0;
        }
    }

    virtual void renameErr(uint8_t core, RenameError err, uint64_t pc, uint8_t iType,
                           int isException, int isInterrupt, uint8_t trapVal,
                           uint16_t specBits, int specTagValid, uint8_t specTag) {
        fprintf(stderr, "[RENAME DEBUG renameErr] core %d, err %d, pc %016llx, iType %d, "
                "isException %d, isInterrupt %d, trapVal %d, "
                "specBits %04x, specTagValid %d, specTag %d\n",
                (int)core, (int)err, (long long unsigned)pc, (int)iType,
                isException, isInterrupt, (int)trapVal,
                (unsigned)specBits, specTagValid, (int)specTag);
        fprintf(log_fp, "[RENAME DEBUG renameErr] core %d, err %d, pc %016llx, iType %d, "
                "isException %d, isInterrupt %d, trapVal %d, "
                "specBits %04x, specTagValid %d, specTag %d\n",
                (int)core, (int)err, (long long unsigned)pc, (int)iType,
                isException, isInterrupt, (int)trapVal,
                (unsigned)specBits, specTagValid, (int)specTag);
        fflush(log_fp);
    }
};
