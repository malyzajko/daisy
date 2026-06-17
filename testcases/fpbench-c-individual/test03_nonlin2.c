#include <fenv.h>
#include <math.h>
#include <stdint.h>
#define TRUE 1
#define FALSE 0

void __PRECOND(int expr);

double test03_nonlin2(double x, double y) {
    __PRECOND((0.0 < x) && (x < 1.0) && (-1.0 < y) && (y < -0.1));
	return (x + y) / (x - y);
}
