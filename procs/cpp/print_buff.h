#pragma once
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <assert.h>

// circular output buffer
class PrintBuffer {
public:
    static const size_t output_buff_lines = 256;
    static const size_t output_buff_line_length = 1024;//512;

private:
    // buffer of lines
    size_t output_buff_index = 0; // next index to use
    bool output_buff_full = false; // for keeping track of short examples
    char output_buff[output_buff_lines][output_buff_line_length];

    // output debug file
    FILE *debug_file;

public:
    PrintBuffer(FILE* dbg_file);
    ~PrintBuffer() {}
    void add_line(const char *fmt, ...);
    void flush_print();
};
