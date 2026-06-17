#include <fenv.h>
#include <math.h>
#include <stdint.h>
#define TRUE 1
#define FALSE 0

void __PRECOND(int expr);

double doppler2(double u, double v, double T) {
    __PRECOND((-125.0 <= u) && (u <= 125.0) && (15.0 <= v) && (v <= 25000.0) && (-40.0 <= T) && (T <= 60.0));
	double t1 = 331.4 + (0.6 * T);
	return (-t1 * v) / ((t1 + u) * (t1 + u));
}
