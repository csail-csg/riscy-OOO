
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

#include "print_buff.h"

PrintBuffer::PrintBuffer(FILE *f) :
    output_buff_index(0),
    output_buff_full(false),
    debug_file(f)
{
    assert(f);
}

void PrintBuffer::add_line(const char *fmt, ...) {
    va_list vl;
    va_start(vl, fmt);
    vsnprintf(output_buff[output_buff_index], output_buff_line_length, fmt, vl);
    va_end(vl);

    if (output_buff_index == output_buff_lines - 1) {
        output_buff_index = 0;
        output_buff_full = true;
    } else {
        output_buff_index++;
    }
}

void PrintBuffer::flush_print() {
    if (output_buff_full) {
        // circular output buffer is full so print the data past the current index first
        for (size_t i = output_buff_index ; i < output_buff_lines ; i++) {
            fprintf(stderr, "%s", output_buff[i]);
            fprintf(debug_file, "%s", output_buff[i]);
        }
    }
    for (size_t i = 0 ; i < output_buff_index ; i++) {
        fprintf(stderr, "%s", output_buff[i]);
        fprintf(debug_file, "%s", output_buff[i]);
    }
    output_buff_full = false;
    output_buff_index = 0;
}
