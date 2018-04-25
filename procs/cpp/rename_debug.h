
// Copyright (c) 2017 Massachusetts Institute of Technology
// 
// Permission is hereby granted, free of charge, to any person
// obtaining a copy of this software and associated documentation
// files (the "Software"), to deal in the Software without
// restriction, including without limitation the rights to use, copy,
// modify, merge, publish, distribute, sublicense, and/or sell copies
// of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
// 
// The above copyright notice and this permission notice shall be
// included in all copies or substantial portions of the Software.
// 
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
// EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
// MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
// NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
// BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
// ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
// CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.

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
                           uint32_t specBits) {
        fprintf(stderr, "[RENAME DEBUG renameErr] core %d, err %d, pc %016llx, iType %d, "
                "isException %d, isInterrupt %d, trapVal %d, specBits %08x\n",
                (int)core, (int)err, (long long unsigned)pc, (int)iType,
                isException, isInterrupt, (int)trapVal, (unsigned)specBits);
        fprintf(log_fp, "[RENAME DEBUG renameErr] core %d, err %d, pc %016llx, iType %d, "
                "isException %d, isInterrupt %d, trapVal %d, specBits %08x\n",
                (int)core, (int)err, (long long unsigned)pc, (int)iType,
                isException, isInterrupt, (int)trapVal, (unsigned)specBits);
        fflush(log_fp);
    }
};
