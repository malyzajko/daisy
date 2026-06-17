#include <fenv.h>
#include <math.h>
#include <stdint.h>
#define TRUE 1
#define FALSE 0

void __PRECOND(int expr);

double rump_with_pow(double a, double b) {
    __PRECOND((70000.0 <= a) && (a <= 80000.0) && (30000.0 <= b) && (b <= 34000.0));
	return ((((333.75 * (((((b * b) * b) * b) * b) * b)) + ((a * a) * (((((11.0 * (a * a)) * (b * b)) - (((((b * b) * b) * b) * b) * b)) - (121.0 * (((b * b) * b) * b))) - 2.0))) + (5.5 * (((((((b * b) * b) * b) * b) * b) * b) * b))) + (a / (2.0 * b)));
}
