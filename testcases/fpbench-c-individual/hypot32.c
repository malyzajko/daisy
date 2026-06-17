#include <fenv.h>
#include <math.h>
#include <stdint.h>
#define TRUE 1
#define FALSE 0

void __PRECOND(int expr);

float hypot32(float x1, float x2) {
    __PRECOND((1.0 <= x1) && (x1 <= 100.0) && (1.0 <= x2) && (x2 <= 100.0));
	return sqrtf(((x1 * x1) + (x2 * x2)));
}
