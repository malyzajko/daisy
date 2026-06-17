#include <fenv.h>
#include <math.h>
#include <stdint.h>
#define TRUE 1
#define FALSE 0

void __PRECOND(int expr);

double exp1x_log(double x) {
    __PRECOND((0.01 <= x) && (x <= 0.5));
	return (exp(x) - 1.0) / log(exp(x));
}
