#include <fenv.h>
#include <math.h>
#include <stdint.h>
#define TRUE 1
#define FALSE 0

void __PRECOND(int expr);

double carthesianToPolar_theta(double x, double y) {
	__PRECOND((1.0 <= x) && (x <= 100.0) && (1.0 <= y) && (y <= 100.0));
	double pi = 3.14159265359;
	double radiant = atan((y / x));
	return radiant * (180.0 / pi);
}
