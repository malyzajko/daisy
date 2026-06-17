#include <fenv.h>
#include <math.h>
#include <stdint.h>
#define TRUE 1
#define FALSE 0

void __PRECOND(int expr);

double doppler1(double u, double v, double T) {
    __PRECOND((-100.0 <= u) && (u <= 100.0) && (20.0 <= v) && (v <= 20000.0) && (-30.0 <= T) && (T <= 50.0));
	double t1 = 331.4 + (0.6 * T);
	return (-t1 * v) / ((t1 + u) * (t1 + u));
}
