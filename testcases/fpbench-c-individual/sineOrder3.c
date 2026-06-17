#include <fenv.h>
#include <math.h>
#include <stdint.h>
#define TRUE 1
#define FALSE 0

void __PRECOND(int expr);

double sineOrder3(double x) {
	__PRECOND((-2.0 < x) && (x < 2.0));
	return (0.954929658551372 * x) - (0.12900613773279798 * ((x * x) * x));
}
