#include <fenv.h>
#include <math.h>
#include <stdint.h>
#define TRUE 1
#define FALSE 0

void __PRECOND(int expr);

double test05_nonlin1_test2(double x) {
    __PRECOND((1.00001 < x) && (x < 2.0));
	return 1.0 / (x + 1.0);
}
