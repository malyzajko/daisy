#include <fenv.h>
#include <math.h>
#include <stdint.h>
#define TRUE 1
#define FALSE 0

void __PRECOND(int expr);

double logexp(double x) {
    __PRECOND((-8.0 <= x) && (x <= 8.0));
	double e = exp(x);
	return log((1.0 + e));
}
