#include <fenv.h>
#include <math.h>
#include <stdint.h>
#define TRUE 1
#define FALSE 0

void __PRECOND(int expr);

double cav10(double x) {
	__PRECOND((0.0 < x) && (x < 10.0));
	double tmp = (((x * x) - x) >= 0.0) ? x / 10.0 : (x * x) + 2.0;
	return tmp;
}
