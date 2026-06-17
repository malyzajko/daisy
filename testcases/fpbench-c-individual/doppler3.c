#include <fenv.h>
#include <math.h>
#include <stdint.h>
#define TRUE 1
#define FALSE 0

void __PRECOND(int expr);

double doppler3(double u, double v, double T) {
    __PRECOND((-30.0 <= u) && (u <= 120.0) && (320.0 <= v) && (v <= 20300.0) && (-50.0 <= T) && (T <= 30.0));
	double t1 = 331.4 + (0.6 * T);
	return (-t1 * v) / ((t1 + u) * (t1 + u));
}
