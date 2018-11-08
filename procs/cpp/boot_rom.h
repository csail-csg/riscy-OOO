#include <vector>
#include <string>
#include <thread>
#include <semaphore.h>
#include "BootRomRequest.h"
#include "BootRomIndication.h"

class BootRomIndication : public BootRomIndicationWrapper {
public:
    BootRomIndication(int id,
                      const char *rom_program_file,
                      int core_num, uint64_t mem_size);
    ~BootRomIndication() {}

    void init_boot_rom(BootRomRequestProxy *req);

    virtual void initDone() { sem_post(&done_sem); }

private:
    BootRomRequestProxy *reqProxy;
    const char *rom_program;
    const int core_num;
    const uint64_t mem_size;
    sem_t done_sem; // signal init done
    
    // create reset vector (from rom_program) at start of boot rom
    void make_reset_vec(std::vector<char> &rom); 
    // create device tree to append to end of boot rom
    void make_dtb(std::vector<char> &rom);

    // get flattened device tree by calling dtc
    static std::string dts_compile(const std::string& dts);
};
