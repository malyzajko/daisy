#include <fenv.h>
#include <math.h>
#include <stdint.h>
#define TRUE 1
#define FALSE 0

double ex0(double a, double b, double c, double d) {
	return sqrt((pow((a + d), 2.0) + pow((b - c), 2.0))) - sqrt((pow((a - d), 2.0) + pow((b + c), 2.0)));
}

