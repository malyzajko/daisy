#include <fenv.h>
#include <math.h>
#include <stdint.h>
#define TRUE 1
#define FALSE 0

void __PRECOND(int expr);

double verhulst(double x) {
    __PRECOND((0.1 <= x) && (x <= 0.3));
	double r = 4.0;
	double K = 1.11;
	return (r * x) / (1.0 + (x / K));
}
