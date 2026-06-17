#include <fenv.h>
#include <math.h>
#include <stdint.h>
#define TRUE 1
#define FALSE 0

void __PRECOND(int expr);

float exp1x_32(float x) {
    __PRECOND((0.01 <= x) && (x <= 0.5));
	return (expf(x) - 1.0f) / x;
}
