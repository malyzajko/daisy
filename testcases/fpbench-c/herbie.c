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

double complex_sine_cosine(double re, double im) {
	__PRECOND((re >= -10.0) && (re <= 10.0) && (im >= -10.0) && (im <= 10.0));
	return (0.5 * sin(re)) * (exp(-im) - exp(im));
}

// double ex2(double cp, double cn, double t, double s) {
// 	return (pow((1.0 / (1.0 + exp(-s))), cp) * pow((1.0 - (1.0 / (1.0 + exp(-s)))), cn)) / (pow((1.0 / (1.0 + exp(-t))), cp) * pow((1.0 - (1.0 / (1.0 + exp(-t)))), cn));
// }

