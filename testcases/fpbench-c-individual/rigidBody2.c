#include <fenv.h>
#include <math.h>
#include <stdint.h>
#define TRUE 1
#define FALSE 0

void __PRECOND(int expr);

double rigidBody2(double x1, double x2, double x3) {
    __PRECOND((-15.0 <= x1) && (x1 <= 15.0) && (-15.0 <= x2) && (x2 <= 15.0) && (-15.0 <= x3) && (x3 <= 15.0));
	return ((((((2.0 * x1) * x2) * x3) + ((3.0 * x3) * x3)) - (((x2 * x1) * x2) * x3)) + ((3.0 * x3) * x3)) - x2;
}
