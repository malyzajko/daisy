#include <fenv.h>
#include <math.h>
#include <stdint.h>
#define TRUE 1
#define FALSE 0

void __PRECOND(int expr);

double floudas(double x1, double x2) {
    __PRECOND((0.0 <= x1) && (x1 <= 2.0) && (0.0 <= x2) && (x2 <= 3.0) && ((x1 + x2) <= 2.0));
	return x1 + x2;
}
