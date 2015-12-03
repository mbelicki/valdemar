#include <stdio.h>
#include <math.h>

extern double add_sq(double a, double b);
extern double sq(double a);
extern double inc(double a);
extern double cos_sq(double a);
extern double locals(void);
extern void side_effects(double a);

extern void print_float(double a) {
    printf("%f\n", a);
}

int main(int argc, char **argv) {
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
}
