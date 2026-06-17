#include <fenv.h>
#include <math.h>
#include <stdint.h>
#define TRUE 1
#define FALSE 0

void __PRECOND(int expr);

float x_by_xy(float x, float y) {
	__PRECOND((1.0 <= x) && (x <= 4.0) && (1.0 <= y) && (y <= 4.0));
	return x / (x + y);
}
