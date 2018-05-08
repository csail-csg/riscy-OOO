#include <stdio.h>
#include "riscv_custom.h"

int main() {
    riscv_roi_begin();
    printf("Hello world\n");
    riscv_roi_end();
    while(1);
    return 0;
}
