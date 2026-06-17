#include <fenv.h>
#include <math.h>
#include <stdint.h>
#define TRUE 1
#define FALSE 0

void __PRECOND(int expr);

double intro_example(double t) {
    __PRECOND((0.0 <= t) && (t <= 999.0));
	return t / (t + 1.0);
}
