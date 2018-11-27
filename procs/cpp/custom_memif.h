#pragma once

#include "fesvr/memif.h"
#include <string.h>
#include <stdlib.h>

// dummy memif which does nothing
class dummy_memif_t : public memif_t
{
public:
  dummy_memif_t() : memif_t(nullptr) {}
  virtual ~dummy_memif_t() {}

  // read and write byte arrays
  virtual void read(addr_t addr, size_t len, void* bytes) {}
  virtual void write(addr_t addr, size_t len, const void* bytes) {}
};

// memif to wrap up a blob of bytes
class blob_memif_t : public memif_t
{
public:
  blob_memif_t(char *bytes, addr_t b, size_t sz)
   : memif_t(nullptr), blob(bytes), base(b), size(sz) {}
  virtual ~blob_memif_t() {}

  // read and write byte arrays
  virtual void read(addr_t addr, size_t len, void* bytes) {
    if (addr < base) {
      fprintf(stderr, "blob memif read underflow\n");
      exit(-1);
    }
    if (addr - base + len > size) {
      fprintf(stderr, "blob memif read overflow\n");
      exit(-1);
    }
    memcpy(bytes, blob + addr - base, len);
  }
  virtual void write(addr_t addr, size_t len, const void* bytes) {
    if (addr < base) {
      fprintf(stderr, "blob memif write underflow\n");
      exit(-1);
    }
    if (addr - base + len > size) {
      fprintf(stderr, "blob memif write overflow\n");
      exit(-1);
    }
    memcpy(blob + addr - base, bytes, len);
  }

private:
  char *blob;
  addr_t base; // we subtract base from any intput addr
  size_t size;
};
