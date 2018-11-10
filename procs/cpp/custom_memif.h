#pragma once

#include "fesvr/memif.h"
#include <string.h>

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
      throw std::runtime_error("blob memif read underflow");
    }
    if (addr - base + len > size) {
      throw std::runtime_error("blob memif read overflow");
    }
    memcpy(bytes, blob + addr - base, len);
  }
  virtual void write(addr_t addr, size_t len, const void* bytes) {
    if (addr < base) {
      throw std::runtime_error("blob memif write underflow");
    }
    if (addr - base + len > size) {
      throw std::runtime_error("blob memif write overflow");
    }
    memcpy(blob + addr - base, bytes, len);
  }

private:
  char *blob;
  addr_t base; // we subtract base from any intput addr
  size_t size;
};
