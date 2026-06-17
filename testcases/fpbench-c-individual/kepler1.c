#include <fenv.h>
#include <math.h>
#include <stdint.h>
#define TRUE 1
#define FALSE 0

void __PRECOND(int expr);

double kepler1(double x1, double x2, double x3, double x4) {
    __PRECOND((4.0 <= x1) && (x1 <= 6.36) && (4.0 <= x2) && (x2 <= 6.36) && (4.0 <= x3) && (x3 <= 6.36) && (4.0 <= x4) && (x4 <= 6.36));
	return (((((((x1 * x4) * (((-x1 + x2) + x3) - x4)) + (x2 * (((x1 - x2) + x3) + x4))) + (x3 * (((x1 + x2) - x3) + x4))) - ((x2 * x3) * x4)) - (x1 * x3)) - (x1 * x2)) - x4;
}
