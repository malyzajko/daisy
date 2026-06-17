#include <fenv.h>
#include <math.h>
#include <stdint.h>
#define TRUE 1
#define FALSE 0

void __PRECOND(int expr);

double nonlin2(double x, double y) {
	__PRECOND((1.001 <= x) && (x <= 2.0) && (1.001 <= y) && (y <= 2.0));
	double t = x * y;
	return (t - 1.0) / ((t * t) - 1.0);
}
