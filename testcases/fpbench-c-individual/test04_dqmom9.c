#include <fenv.h>
#include <math.h>
#include <stdint.h>
#define TRUE 1
#define FALSE 0

void __PRECOND(int expr);

double test04_dqmom9(double m0, double m1, double m2, double w0, double w1, double w2, double a0, double a1, double a2) {
    __PRECOND((-1.0 < m0) && (m0 < 1.0) && (-1.0 < m1) && (m1 < 1.0) && (-1.0 < m2) && (m2 < 1.0) && (1.0e-05 < w0) && (w0 < 1.0) && (1.0e-05 < w1) && (w1 < 1.0) && (1.0e-05 < w2) && (w2 < 1.0) && (1.0e-05 < a0) && (a0 < 1.0) && (1.0e-05 < a1) && (a1 < 1.0) && (1.0e-05 < a2) && (a2 < 1.0));
	double v2 = (w2 * (0.0 - m2)) * (-3.0 * ((1.0 * (a2 / w2)) * (a2 / w2)));
	double v1 = (w1 * (0.0 - m1)) * (-3.0 * ((1.0 * (a1 / w1)) * (a1 / w1)));
	double v0 = (w0 * (0.0 - m0)) * (-3.0 * ((1.0 * (a0 / w0)) * (a0 / w0)));
	return 0.0 + ((v0 * 1.0) + ((v1 * 1.0) + ((v2 * 1.0) + 0.0)));
}
