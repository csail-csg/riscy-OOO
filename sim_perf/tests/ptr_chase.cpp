#include "riscv_custom.h"

int main() {
    const int len = 1024;
    int data[len];
    for(int i = 0; i < len - 1; i++) {
        data[i] = i + 1;
    }
    data[len - 1] = 0;

    riscv_roi_begin();

    int v = 0;
    for(int i = 0; i < 10000000; i++) {
        v = data[v];
    }
    riscv_roi_end();

    while(v + 1);
    return v;
}
