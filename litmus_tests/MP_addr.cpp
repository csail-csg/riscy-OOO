#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include <string>
#include <stdint.h>

volatile uint64_t start0 __attribute__ ((aligned (64))) = 0;
volatile uint64_t start1 __attribute__ ((aligned (64))) = 0;

volatile uint64_t flag __attribute__ ((aligned (64))) = 0;
volatile uint64_t data __attribute__ ((aligned (64))) = 0;

volatile int sync __attribute__ ((aligned (64))) = 0;

volatile int zero __attribute__ ((aligned (64))) = 0;

unsigned long long test_num = 0;
int delay_range_st = 10; // default 10
unsigned long long count_fd[4] = {0, 0, 0, 0};

// t0: St data = 1; Fence; St flag = 1
// t1: St flag = 1; Ld (data + (flag >> 1)) = 0

void *thread0(void *p) {
    for (unsigned long long i = 0; i < test_num; i++) {
        while (!start0) {
            __sync_synchronize();
        }
        __sync_synchronize();

        // prepare
        int delay = abs(rand()) % delay_range_st;
        flag = 0; // fetch flag to cache

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
        data = 1;
        __sync_synchronize();
        flag = 1;

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
        data = 0; // fetch data to cache

        // wait for the other core to be ready
        __sync_fetch_and_add(&sync, 1);
        while (sync != 2) {
            __sync_synchronize();
        }
        __sync_synchronize();
        
        // fake pointer chasing to delay the address computation of flag
        int temp = 0;
        temp = *(&zero + temp);
        temp = *(&zero + temp);
        temp = *(&zero + temp);
        temp = *(&zero + temp);

        // real test
        uint64_t f = *(&flag + temp);
        uint64_t d = *(&data + (f >> 1));

        count_fd[((f & 0x01ULL) << 1) | (d & 0x01ULL)]++;

        // signal done
        __sync_synchronize();
        start1 = 0;
    }

    return 0;
}

void *thread2(void *p) {
    while (true) {
        zero = 0;
    }
    return 0;
}

int main(int argc, char **argv) {
    if (argc < 2) {
        fprintf(stderr, "Usage: %s TEST_NUM [DELAY_RANGE_ST=10]\n", argv[0]);
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
        delay_range_st = atoi(argv[2]);
    }

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
    pthread_t t2;
    {
        pthread_attr_t attr;
        pthread_attr_init(&attr);
        cpu_set_t cpu;
        CPU_ZERO(&cpu);
        CPU_SET(3, &cpu); // t2 - core 3
        int r = pthread_attr_setaffinity_np(&attr, sizeof(cpu_set_t), &cpu);
        if (r) {
            fprintf(stderr, "fail to set affinity for t2\n");
            exit(-1);
        }
        r = pthread_create(&t2, &attr, thread2, NULL);
        if (r) {
            fprintf(stderr, "fail to create t2\n");
            exit(-1);
        }
        pthread_attr_destroy(&attr);
    }


    for (unsigned long long i = 0; i < test_num; i++) {
        flag = 0;
        data = 0;
        sync = 0;
        __sync_synchronize();
        start0 = 1;
        start1 = 1;

        while (start0 || start1)  {
            __sync_synchronize();
        }
        __sync_synchronize();
    }

    printf("test num %llu\n", test_num);
    printf("f 0 d 0: %llu \n", count_fd[0]);
    printf("f 0 d 1: %llu \n", count_fd[1]);
    printf("f 1 d 0: %llu \n", count_fd[2]);
    printf("f 1 d 1: %llu \n", count_fd[3]);
    return 0;
}
