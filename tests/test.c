#include <stdio.h>

extern double add_sq(double a, double b);
extern double sq(double a);
extern double inc(double a);

int main(int argc, char **argv) {
    printf("add_sq(1, 2) = %f\n", add_sq(1, 2));
    printf("sq(2) = %f\n", sq(2));
    printf("inc(2) = %f\n", inc(2));
}
