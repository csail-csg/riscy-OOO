#include <iostream>
#include <sstream>
#include <errno.h>
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <poll.h>
#include <sys/wait.h>
#include <sys/types.h>
#include "custom_memif.h"
#include "fesvr/elfloader.h" // FIXME must be after memif for reg_t definition
#include "boot_rom.h"
#include "mmio.h"

BootRomIndication::BootRomIndication(int id,
                                     const char *rom_prog,
                                     int cores, uint64_t memsz) :
    BootRomIndicationWrapper(id),
    reqProxy(nullptr), rom_program(rom_prog),
    core_num(cores), mem_size(memsz)
{
    sem_init(&done_sem, 0, 0);
}

void BootRomIndication::init_boot_rom(BootRomRequestProxy *req) {
    // record req proxy, and set it to keep retrying on failure
    reqProxy = req;
    reqProxy->pint.busyType = BUSY_SPIN;

    // setup contents of rom
    std::vector<char> rom;
    make_reset_vec(rom);
    make_dtb(rom);

    // send rom contents to FPGA
    if((uint64_t)(&rom[0]) % 8) {
        fprintf(stderr, "[boot rom] ERROR: "
                "rom start addr %llx not algined to 8B\n",
                (long long unsigned)(&rom[0]));
        exit(1);
    }
    if(rom.size() % 8) {
        fprintf(stderr, "[boot rom] ERROR: rom size %d not algined to 8B\n",
                (int)(rom.size()));
        exit(1);
    }
    uint64_t *ptr = (uint64_t*)(&rom[0]);
    int data_count = rom.size() / 8;
    for(int i = 0; i < data_count; i++) {
        reqProxy->initReq(i, ptr[i], i == data_count - 1);
    }

    // wait done
    sem_wait(&done_sem);
    fprintf(stderr, "[boot rom] inited\n");
}

void BootRomIndication::make_reset_vec(std::vector<char> &rom) {
    // load rom program here
    // first figure out the size
    dummy_memif_t dummy_memif;
    uint64_t start_addr;
    std::map<std::string, uint64_t> symbols = load_elf(rom_program,
                                                       &dummy_memif,
                                                       &start_addr);
    auto it = symbols.find("_end");
    if (it == symbols.end()) {
        fprintf(stderr, "[boot rom] No symbol _end in rom program");
        exit(1);
    }
    uint64_t end_addr = it->second;
    uint64_t reset_vec_size = end_addr - start_addr;
    fprintf(stderr, "[boot rom] rom program: "
            "start %016llx, end %016llx, size %d\n",
            (long long unsigned)start_addr,
            (long long unsigned)end_addr,
            (int)reset_vec_size);
    // now do the real copy
    rom.resize(reset_vec_size, 0);
    blob_memif_t blob_memif(&(rom[0]), start_addr, reset_vec_size);
    load_elf(rom_program, &blob_memif, &start_addr);
}

void BootRomIndication::make_dtb(std::vector<char> &rom)
{
    // then make the device tree
    size_t timebase_freq = 10000000; // 10MHz
    size_t cpu_freq = 1000000000; // 1GHz
    std::string isa_str = "rv64imafd";
    std::string vm_mode = "sv39";
    // addresses copied from spike
    reg_t clint_base = CLINT_BASE;
    reg_t clint_size = CLINT_SIZE;
    reg_t mem_base   = MAIN_MEM_BASE;

    std::stringstream s;
    s << std::dec <<
             "/dts-v1/;\n"
             "\n"
             "/ {\n"
             "  #address-cells = <2>;\n"
             "  #size-cells = <2>;\n"
             "  compatible = \"ucbbar,spike-bare-dev\";\n"
             "  model = \"ucbbar,spike-bare\";\n"
             "  cpus {\n"
             "    #address-cells = <1>;\n"
             "    #size-cells = <0>;\n"
             "    timebase-frequency = <" << timebase_freq << ">;\n";
    for (int i = 0; i < core_num; i++) {
        s << "    CPU" << i << ": cpu@" << i << " {\n"
             "      device_type = \"cpu\";\n"
             "      reg = <" << i << ">;\n"
             "      status = \"okay\";\n"
             "      compatible = \"riscv\";\n"
             "      riscv,isa = \"" << isa_str << "\";\n"
             "      mmu-type = \"riscv," << vm_mode << "\";\n"
             "      clock-frequency = <" << cpu_freq << ">;\n"
             "      CPU" << i << "_intc: interrupt-controller {\n"
             "        #interrupt-cells = <1>;\n"
             "        interrupt-controller;\n"
             "        compatible = \"riscv,cpu-intc\";\n"
             "      };\n"
             "    };\n";
    }
    s <<     "  };\n";
    s << std::hex <<
             "  memory@" << mem_base << " {\n"
             "    device_type = \"memory\";\n"
             "    reg = <0x" << (mem_base >> 32) << " 0x" << (mem_base & (uint32_t)-1) <<
                       " 0x" << (mem_size >> 32) << " 0x" << (mem_size & (uint32_t)-1) << ">;\n"
             "  };\n";
    s <<     "  soc {\n"
             "    #address-cells = <2>;\n"
             "    #size-cells = <2>;\n"
             "    compatible = \"ucbbar,spike-bare-soc\", \"simple-bus\";\n"
             "    ranges;\n"
             "    clint@" << clint_base << " {\n"
             "      compatible = \"riscv,clint0\";\n"
             "      interrupts-extended = <" << std::dec;
    for (int i = 0; i < core_num; i++)
        s << "&CPU" << i << "_intc 3 &CPU" << i << "_intc 7 ";
    s << std::hex << ">;\n"
             "      reg = <0x" << (clint_base >> 32) << " 0x" << (clint_base & (uint32_t)-1) <<
                         " 0x" << (clint_size >> 32) << " 0x" << (clint_size & (uint32_t)-1) << ">;\n"
             "    };\n"
             "  };\n"
             "};\n";

    std::string dts = s.str();
    fprintf(stderr, "[boot rom] dts\n%s", dts.c_str());
    std::string dtb = dts_compile(dts);

    rom.insert(rom.end(), dtb.begin(), dtb.end());
    const int align = 0x1000;
    rom.resize((rom.size() + align - 1) / align * align);

    fprintf(stderr, "[boot rom] final size = %d\n", int(rom.size()));
}

// creat device tree by calling dtc (copied from spike)
std::string BootRomIndication::dts_compile(const std::string& dts)
{
  // Convert the DTS to DTB
  int dts_pipe[2];
  pid_t dts_pid;

  if (pipe(dts_pipe) != 0 || (dts_pid = fork()) < 0) {
    std::cerr << "Failed to fork dts child: " << strerror(errno) << std::endl;
    exit(1);
  }

  // Child process to output dts
  if (dts_pid == 0) {
    close(dts_pipe[0]);
    int step, len = dts.length();
    const char *buf = dts.c_str();
    for (int done = 0; done < len; done += step) {
      step = write(dts_pipe[1], buf+done, len-done);
      if (step == -1) {
        std::cerr << "Failed to write dts: " << strerror(errno) << std::endl;
        exit(1);
      }
    }
    close(dts_pipe[1]);
    exit(0);
  }

  pid_t dtb_pid;
  int dtb_pipe[2];
  if (pipe(dtb_pipe) != 0 || (dtb_pid = fork()) < 0) {
    std::cerr << "Failed to fork dtb child: " << strerror(errno) << std::endl;
    exit(1);
  }

  // Child process to output dtb
  const char *dtc_bin = DTC_PATH;
  if (dtb_pid == 0) {
    dup2(dts_pipe[0], 0);
    dup2(dtb_pipe[1], 1);
    close(dts_pipe[0]);
    close(dts_pipe[1]);
    close(dtb_pipe[0]);
    close(dtb_pipe[1]);
    execl(dtc_bin, dtc_bin, "-O", "dtb", 0);
    std::cerr << "Failed to run " << dtc_bin << ": " << strerror(errno) << std::endl;
    exit(1);
  }

  close(dts_pipe[1]);
  close(dts_pipe[0]);
  close(dtb_pipe[1]);

  // Read-out dtb
  std::stringstream dtb;

  int got;
  char buf[4096];
  while ((got = read(dtb_pipe[0], buf, sizeof(buf))) > 0) {
    dtb.write(buf, got);
  }
  if (got == -1) {
    std::cerr << "Failed to read dtb: " << strerror(errno) << std::endl;
    exit(1);
  }
  close(dtb_pipe[0]);

  // Reap children
  int status;
  waitpid(dts_pid, &status, 0);
  if (!WIFEXITED(status) || WEXITSTATUS(status) != 0) {
    std::cerr << "Child dts process failed" << std::endl;
    exit(1);
  }
  waitpid(dtb_pid, &status, 0);
  if (!WIFEXITED(status) || WEXITSTATUS(status) != 0) {
    std::cerr << "Child dtb process failed" << std::endl;
    exit(1);
  }

  return dtb.str();
}

