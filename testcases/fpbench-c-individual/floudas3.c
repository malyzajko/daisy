#include <fenv.h>
#include <math.h>
#include <stdint.h>
#define TRUE 1
#define FALSE 0

void __PRECOND(int expr);

double floudas3(double x1, double x2) {
    __PRECOND((0.0 <= x1) && (x1 <= 2.0) && (0.0 <= x2) && (x2 <= 3.0) && (((-2.0 * ((x1 * x1) * (x1 * x1))) + 2.0) >= x2));
	return ((-12.0 * x1) - (7.0 * x2)) + (x2 * x2);
}
