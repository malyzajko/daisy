#include <fenv.h>
#include <math.h>
#include <stdint.h>
#define TRUE 1
#define FALSE 0

void __PRECOND(int expr);

double himmilbeau(double x1, double x2) {
    __PRECOND((-5.0 <= x1) && (x1 <= 5.0) && (-5.0 <= x2) && (x2 <= 5.0));
	double a = ((x1 * x1) + x2) - 11.0;
	double b = (x1 + (x2 * x2)) - 7.0;
	return (a * a) + (b * b);
}
