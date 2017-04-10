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
