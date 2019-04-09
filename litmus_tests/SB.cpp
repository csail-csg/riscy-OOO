#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include <string>

volatile int start0 __attribute__ ((aligned (64))) = 0;
volatile int start1 __attribute__ ((aligned (64))) = 0;

volatile int res_a __attribute__ ((aligned (64))) = 0;
volatile int res_b __attribute__ ((aligned (64))) = 0;

volatile int sync __attribute__ ((aligned (64))) = 0;

volatile int a __attribute__ ((aligned (64))) = 0;
volatile int b __attribute__ ((aligned (64))) = 0;

unsigned long long test_num = 0;
int delay_range = 10; // default 10

// t0: St a = 1; Ld b = 0
// t1: St b = 1; Ld a = 0

void *thread0(void *p) {
    for (unsigned long long i = 0; i < test_num; i++) {
        while (!start0) {
            __sync_synchronize();
        }
        __sync_synchronize();

        // prepare
        int delay = abs(rand()) % delay_range;
        b = 0; // fetch b to cache

        // wait for the other core to be ready
        __sync_fetch_and_add(&sync, 1);
        while (sync != 2) {
            __sync_synchronize();
        }
        __sync_synchronize();
        
        // some random delay
        for (int i = 0; i < delay; i++) {
            asm volatile ("nop");
        }

        // real test
        a = 1;
        res_b = b;

        // signal done
        __sync_synchronize();
        start0 = 0;
    }

    return 0;
}

void *thread1(void *res) {
    for (unsigned long long i = 0; i < test_num; i++) {
        while (!start1) {
            __sync_synchronize();
        }
        __sync_synchronize();

        // prepare
        int delay = abs(rand()) % delay_range;
        a = 0; // fetch a to cache

        // wait for the other core to be ready
        __sync_fetch_and_add(&sync, 1);
        while (sync != 2) {
            __sync_synchronize();
        }
        __sync_synchronize();
        
        // some random delay
        for (int i = 0; i < delay; i++) {
            asm volatile ("nop");
        }

        // real test
        b = 1;
        res_a = a;

        // signal done
        __sync_synchronize();
        start1 = 0;
    }

    return 0;
}

int main(int argc, char **argv) {
    if (argc < 2) {
        fprintf(stderr, "Usage: %s TEST_NUM [DELAY_RANGE=10]\n", argv[0]);
        return 0;
    }

    {
        cpu_set_t cpu;
        CPU_ZERO(&cpu);
        CPU_SET(0, &cpu); // main - core 0
        pthread_t self = pthread_self();
        int r = pthread_setaffinity_np(self, sizeof(cpu_set_t), &cpu);
        if (r) {
            fprintf(stderr, "fail to set affinity for main thread\n");
            exit(-1);
        }
    }

    test_num = std::stoull(argv[1]);
    if (argc >= 3) {
        delay_range = atoi(argv[2]);
    }

    long long unsigned count_ab[4] = {0, 0, 0, 0};

    pthread_t t0;
    {
        pthread_attr_t attr;
        pthread_attr_init(&attr);
        cpu_set_t cpu;
        CPU_ZERO(&cpu);
        CPU_SET(1, &cpu); // t0 - core 1
        int r = pthread_attr_setaffinity_np(&attr, sizeof(cpu_set_t), &cpu);
        if (r) {
            fprintf(stderr, "fail to set affinity for t0\n");
            exit(-1);
        }
        r = pthread_create(&t0, &attr, thread0, NULL);
        if (r) {
            fprintf(stderr, "fail to create t0\n");
            exit(-1);
        }
        pthread_attr_destroy(&attr);
    }
    pthread_t t1;
    {
        pthread_attr_t attr;
        pthread_attr_init(&attr);
        cpu_set_t cpu;
        CPU_ZERO(&cpu);
        CPU_SET(2, &cpu); // t1 - core 2
        int r = pthread_attr_setaffinity_np(&attr, sizeof(cpu_set_t), &cpu);
        if (r) {
            fprintf(stderr, "fail to set affinity for t1\n");
            exit(-1);
        }
        r = pthread_create(&t1, &attr, thread1, NULL);
        if (r) {
            fprintf(stderr, "fail to create t1\n");
            exit(-1);
        }
        pthread_attr_destroy(&attr);
    }

    for (unsigned long long i = 0; i < test_num; i++) {
        res_a = 1;
        res_b = 1;
        a = 0;
        b = 0;
        sync = 0;
        __sync_synchronize();
        start0 = 1;
        start1 = 1;

        while (start0 || start1)  {
            __sync_synchronize();
        }
        __sync_synchronize();

        count_ab[((res_a & 0x01) << 1) | (res_b & 0x01)]++;
    }

    printf("test num %llu\n", test_num);
    printf("a 0 b 0: %llu\n", count_ab[0]);
    printf("a 0 b 1: %llu\n", count_ab[1]);
    printf("a 1 b 0: %llu\n", count_ab[2]);
    printf("a 1 b 1: %llu\n", count_ab[3]);
    return 0;
}
