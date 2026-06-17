#include <fenv.h>
#include <math.h>
#include <stdint.h>
#define TRUE 1
#define FALSE 0

void __PRECOND(int expr);

double polarToCarthesian_y(double radius, double theta) {
	__PRECOND((1.0 <= radius) && (radius <= 10.0) && (0.0 <= theta) && (theta <= 360.0));
	double pi = 3.14159265359;
	double radiant = theta * (pi / 180.0);
	return radius * sin(radiant);
}
