#include <stdio.h>
#include <math.h>
#include <stdbool.h>

extern double add_sq(double a, double b);
extern double sq(double a);
extern double inc(double a);
extern double cos_sq(double a);
extern double locals(void);
extern void side_effects(double a);
extern bool booleans(bool a, bool b);
extern void branching(double a, bool more);
extern void count_down(double a);
extern double vald_fabs(double a);
extern bool compare_float(double a, double b, double eps);
extern void mutability(void);

extern void print_float(double a) {
    printf("%f\n", a);
}

int test_main(int argc, char **argv) {
    puts("-- basics: --");
    printf("add_sq(1, 2) = %f\n", add_sq(1, 2));
    printf("sq(2) = %f\n", sq(2));
    printf("inc(2) = %f\n", inc(2));
    
    puts("-- calling c: --");
    printf("cos(2) * cos(2) = %f\n", cos(2) * cos(2));
    printf("      cos_sq(2) = %f\n", cos_sq(2));

    puts("-- locals: --");
    printf("locals() = %f\n", locals());

    puts("-- void: --");
    puts("calling side_effects(8)");
    side_effects(8);

    puts("-- booleans: --");
    printf("booleans(true, false) = %s\n", booleans(1, 0) ? "true" : "false");
    printf("true || false && false = %s\n", (true || false && false) ? "true" : "false");

    puts("-- basic branching: --");
    puts("calling branching(8, true)");
    branching(8, true);
    puts("calling branching(8, false)");
    branching(8, false);
    puts("calling count_down(8)");
    count_down(8);
    printf("     fabs(-3.14) = %f\n", fabs(-3.14));
    printf("vald_fabs(-3.14) = %f\n", vald_fabs(-3.14));
    printf( "compare_float(3.14, 3.135, 0.01) = %s\n"
          , compare_float(3.14, 3.135, 0.01) ? "true" : "false"
          );

    puts("-- muatbility: --");
    puts("calling muatbility()");
    mutability();
}
