#include <fenv.h>
#include <math.h>
#include <stdint.h>
#define TRUE 1
#define FALSE 0

void __PRECOND(int expr);

double bspline3(double u) {
	__PRECOND((0.0 <= u) && (u <= 1.0));
	return -((u * u) * u) / 6.0;
}
