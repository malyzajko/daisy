#include <fenv.h>
#include <math.h>
#include <stdint.h>
#define TRUE 1
#define FALSE 0

void __PRECOND(int expr);

double floudas2(double x1, double x2) {
    __PRECOND((0.0 <= x1) && (x1 <= 3.0) && (0.0 <= x2) && (x2 <= 4.0) && (((((2.0 * ((x1 * x1) * (x1 * x1))) - ((8.0 * (x1 * x1)) * x1)) + ((8.0 * x1) * x1)) - x2) >= 0.0) && (((((((4.0 * ((x1 * x1) * (x1 * x1))) - ((32.0 * (x1 * x1)) * x1)) + ((88.0 * x1) * x1)) - (96.0 * x1)) + 36.0) - x2) >= 0.0));
	return -x1 - x2;
}
