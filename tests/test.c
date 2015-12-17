#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#include <stdbool.h>
#include <stdint.h>
#include <inttypes.h>

extern void print_float(double a) {
    printf("%f\n", a);
}

extern void print_int(int64_t a) {
    printf("%" PRId64 "\n", a);
}

extern void print_char(char c) {
    putchar(c);
}

extern void bounds_check_failed(void) {
    fprintf(stderr, "Array out of bounds.");
    abort();
}

