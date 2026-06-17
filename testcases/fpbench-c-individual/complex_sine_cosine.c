#include <fenv.h>
#include <math.h>
#include <stdint.h>
#define TRUE 1
#define FALSE 0

void __PRECOND(int expr);

double complex_sine_cosine(double re, double im) {
	__PRECOND((re >= -10.0) && (re <= 10.0) && (im >= -10.0) && (im <= 10.0));
	return (0.5 * sin(re)) * (exp(-im) - exp(im));
}
