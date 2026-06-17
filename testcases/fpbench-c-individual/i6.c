#include <fenv.h>
#include <math.h>
#include <stdint.h>
#define TRUE 1
#define FALSE 0

void __PRECOND(int expr);

float i6(float x, float y) {
    __PRECOND((0.1 <= x) && (x <= 10.0) && (-5.0 <= y) && (y <= 5.0));
	return sinf((x * y));
}
