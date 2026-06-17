#include <fenv.h>
#include <math.h>
#include <stdint.h>
#define TRUE 1
#define FALSE 0

void __PRECOND(int expr);

float test06_sums4_sum2(float x0, float x1, float x2, float x3) {
    __PRECOND((-1.0e-05 < x0) && (x0 < 1.00001) && (0.0 < x1) && (x1 < 1.0) && (0.0 < x2) && (x2 < 1.0) && (0.0 < x3) && (x3 < 1.0));
	return (x0 + x1) + (x2 + x3);
}
