#include <fenv.h>
#include <math.h>
#include <stdint.h>
#define TRUE 1
#define FALSE 0

void __PRECOND(int expr);

double complex_square_root(double re, double im) {
	__PRECOND((re >= 0.001) && (re <= 10.0) && (im >= 0.001) && (im <= 10.0));
	return 0.5 * sqrt((2.0 * (sqrt(((re * re) + (im * im))) + re)));
}
