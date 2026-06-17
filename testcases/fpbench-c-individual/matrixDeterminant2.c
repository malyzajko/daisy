#include <fenv.h>
#include <math.h>
#include <stdint.h>
#define TRUE 1
#define FALSE 0

void __PRECOND(int expr);

double matrixDeterminant2(double a, double b, double c, double d, double e, double f, double g, double h, double i) {
	__PRECOND((-10.0 <= a) && (a <= 10.0) && (-10.0 <= b) && (b <= 10.0) && (-10.0 <= c) && (c <= 10.0) && (-10.0 <= d) && (d <= 10.0) && (-10.0 <= e) && (e <= 10.0) && (-10.0 <= f) && (f <= 10.0) && (-10.0 <= g) && (g <= 10.0) && (-10.0 <= h) && (h <= 10.0) && (-10.0 <= i) && (i <= 10.0));
	return ((((e * i) * a) + (((b * f) * g) + ((d * h) * c))) - (((c * g) * e) + (((b * d) * i) + ((f * h) * a))));
}
