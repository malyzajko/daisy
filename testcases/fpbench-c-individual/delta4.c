#include <fenv.h>
#include <math.h>
#include <stdint.h>
#define TRUE 1
#define FALSE 0

void __PRECOND(int expr);

double delta4(double x1, double x2, double x3, double x4, double x5, double x6) {
	__PRECOND((4.0 <= x1) && (x1 <= 6.3504) && (4.0 <= x2) && (x2 <= 6.3504) && (4.0 <= x3) && (x3 <= 6.3504) && (4.0 <= x4) && (x4 <= 6.3504) && (4.0 <= x5) && (x5 <= 6.3504) && (4.0 <= x6) && (x6 <= 6.3504));
	return (((((-x2 * x3) - (x1 * x4)) + (x2 * x5)) + (x3 * x6)) - (x5 * x6)) + (x1 * (((((-x1 + x2) + x3) - x4) + x5) + x6));
}
