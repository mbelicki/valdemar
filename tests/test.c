#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#include <stdbool.h>
#include <stdint.h>
#include <inttypes.h>

extern void print_float(double a) {
    printf("%f", a);
}

extern void print_int(int64_t a) {
    printf("%" PRId64, a);
}

extern void print_char(char c) {
    putchar(c);
}

extern char get_char() {
    return getchar();
}

static unsigned short Xi[3] = {0, 0, 209};

extern double get_random() {
    return erand48(Xi);
}
