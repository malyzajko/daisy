#include <fenv.h>
#include <math.h>
#include <stdint.h>
#define TRUE 1
#define FALSE 0

void __PRECOND(int expr);

double test02_sum8(double x0, double x1, double x2, double x3, double x4, double x5, double x6, double x7) {
    __PRECOND((1.0 < x0) && (x0 < 2.0) && (1.0 < x1) && (x1 < 2.0) && (1.0 < x2) && (x2 < 2.0) && (1.0 < x3) && (x3 < 2.0) && (1.0 < x4) && (x4 < 2.0) && (1.0 < x5) && (x5 < 2.0) && (1.0 < x6) && (x6 < 2.0) && (1.0 < x7) && (x7 < 2.0));
	return ((((((x0 + x1) + x2) + x3) + x4) + x5) + x6) + x7;
}
