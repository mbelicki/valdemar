#include <stdio.h>
#include <math.h>
#include <stdbool.h>

extern void print_float(double a) {
    printf("%f\n", a);
}

extern void print_char(char c) {
    putchar(c);
}

extern void bounds_check_failed(void) {
    fprintf(stderr, "Array out of bounds.");
    abort();
}

