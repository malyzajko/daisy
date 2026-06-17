#include <fenv.h>
#include <math.h>
#include <stdint.h>
#define TRUE 1
#define FALSE 0

void __PRECOND(int expr);

double turbine2(double v, double w, double r) {
    __PRECOND((-4.5 <= v) && (v <= -0.3) && (0.4 <= w) && (w <= 0.9) && (3.8 <= r) && (r <= 7.8));
	return ((6.0 * v) - (((0.5 * v) * (((w * w) * r) * r)) / (1.0 - v))) - 2.5;
}
