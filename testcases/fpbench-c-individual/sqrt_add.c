#include <fenv.h>
#include <math.h>
#include <stdint.h>
#define TRUE 1
#define FALSE 0

void __PRECOND(int expr);

double sqrt_add(double x) {
    __PRECOND((1.0 <= x) && (x <= 1000.0));
	return 1.0 / (sqrt((x + 1.0)) + sqrt(x));
}
