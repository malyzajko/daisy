#include <fenv.h>
#include <math.h>
#include <stdint.h>
#define TRUE 1
#define FALSE 0

void __PRECOND(int expr);

double nonlin1(double z) {
	__PRECOND((0.0 <= z) && (z <= 999.0));
	return z / (z + 1.0);
}
