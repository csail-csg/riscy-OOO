
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

#include <iostream>
#include <sstream>
#include <string>
#include <unistd.h>
#include <stdexcept>
#include <stdlib.h>
#include <errno.h>
#include <assert.h>
#include <stddef.h>
#include <poll.h>
#include <sys/wait.h>
#include <sys/types.h>
#include "htif_riscy.h"

htif_riscy_t::htif_riscy_t(const std::vector<std::string>& args,
                           uint32_t _core_num) :
    htif_t(args),
    core_num(_core_num),
    mem_size(0),
    mem_base(0x80000000ULL),
    dma_read(nullptr),
    dma_write(nullptr),
    write_from_host(nullptr),
    verbose(false)
{
}

void htif_riscy_t::get_to_host(reg_t x) {
    // The processor just wrote x to the to_host register
    // Hopefully things are initialized
    assert(mem_size > 0);

    if (verbose) fprintf(stderr,
                         "htif_riscy_t::get_to_host(0x%llx)\n",
                         (long long)x);

    command_t cmd(this, x, std::bind(write_from_host, std::placeholders::_1));
    get_device_list().handle_command(cmd);

    if (raw_exitcode_nonzero()) {
        stop();
    }
}

void htif_riscy_t::read_chunk(addr_t taddr, size_t len, void* dst) {
    assert(taddr >= 0);
    assert(taddr + len <= mem_base + mem_size);

    if (verbose) fprintf(stderr,
                         "htif_riscy_t::read_chunk"
                         "(taddr=0x%llx, len=%ld, dst=%p)\n",
                         (long long)taddr, (long)len, dst);
    dma_read(taddr, len, dst);
}

void htif_riscy_t::write_chunk(addr_t taddr, size_t len, const void* src) {
    assert(taddr >= 0);
    assert(taddr + len <= mem_base + mem_size);

    if (verbose) fprintf(stderr,
                         "htif_riscy_t::write_chunk"
                         "(taddr=0x%llx, len=%ld, src=%p)\n",
                         (long long)taddr, (long)len, src);
    dma_write(taddr, len, src);
}

bool htif_riscy_t::bcd_wait_for_stdin() {
    return get_bcd().waiting_for_stdin();
}

void htif_riscy_t::bcd_feed_stdin(int ch) {
    get_bcd().feed_stdin(ch);
}

void htif_riscy_t::reset() {
    std::vector<char> rom;
    make_dtb(rom);
    if((uint64_t)(&rom[0]) % 8) {
        fprintf(stderr, ">> ERROR: rom start addr %llx not algined to 8B\n",
                (long long unsigned)(&rom[0]));
    }
    if(rom.size() % 8) {
        fprintf(stderr, ">> ERROR: rom size %d not algined to 8B\n",
                (int)(rom.size()));
    }
    uint64_t *ptr = (uint64_t*)(&rom[0]);
    int data_count = rom.size() / 8;
    for(int i = 0; i < data_count; i++) {
        write_boot_rom(i, ptr[i]);
        wait_boot_rom();
    }
    fprintf(stderr, "Boot rom inited\n");
}

// creat device tree (copied from spike)
static std::string dts_compile(const std::string& dts)
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
  const char *DTC = "/usr/bin/dtc";
  if (dtb_pid == 0) {
    dup2(dts_pipe[0], 0);
    dup2(dtb_pipe[1], 1);
    close(dts_pipe[0]);
    close(dts_pipe[1]);
    close(dtb_pipe[0]);
    close(dtb_pipe[1]);
    execl(DTC, DTC, "-O", "dtb", 0);
    std::cerr << "Failed to run " << DTC << ": " << strerror(errno) << std::endl;
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

void htif_riscy_t::make_dtb(std::vector<char> &rom)
{
    const int reset_vec_size = 8;
    addr_t start_pc = get_entry_point();
    uint32_t reset_vec[reset_vec_size] = {
      0x297,                                      // auipc  t0,0x0
      0x28593 + (reset_vec_size * 4 << 20),       // addi   a1, t0, &dtb
      0xf1402573,                                 // csrr   a0, mhartid
      0x0182b283u,                                // ld     t0,24(t0)
      0x28067,                                    // jr     t0
      0,
      (uint32_t) (start_pc & 0xffffffff),
      (uint32_t) (start_pc >> 32)
    };

    rom.clear();
    rom.insert(rom.end(), (char*)reset_vec, (char*)reset_vec + sizeof(reset_vec));

    size_t timebase_freq = 10000000; // 10MHz
    size_t cpu_freq = 1000000000; // 1GHz
    std::string isa_str = "rv64imafd";
    std::string vm_mode = "sv39";
    // addresses copied from spike
    reg_t clint_base = 0x02000000;
    reg_t clint_size = 0x000c0000;

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
    for (size_t i = 0; i < core_num; i++) {
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
    for (size_t i = 0; i < core_num; i++)
        s << "&CPU" << i << "_intc 3 &CPU" << i << "_intc 7 ";
    s << std::hex << ">;\n"
             "      reg = <0x" << (clint_base >> 32) << " 0x" << (clint_base & (uint32_t)-1) <<
                         " 0x" << (clint_size >> 32) << " 0x" << (clint_size & (uint32_t)-1) << ">;\n"
             "    };\n"
             "  };\n"
             "};\n";

    std::string dts = s.str();
    fprintf(stderr, ">> INFO: dts\n%s", dts.c_str());
    std::string dtb = dts_compile(dts);

    rom.insert(rom.end(), dtb.begin(), dtb.end());
    const int align = 0x1000;
    rom.resize((rom.size() + align - 1) / align * align);

    fprintf(stderr, ">> INFO: boot rom size = %d\n", int(rom.size()));
}

