#include <fenv.h>
#include <math.h>
#include <stdint.h>
#define TRUE 1
#define FALSE 0

void __PRECOND(int expr);

float test01_sum3(float x0, float x1, float x2) {
    __PRECOND((1.0 < x0) && (x0 < 2.0) && (1.0 < x1) && (x1 < 2.0) && (1.0 < x2) && (x2 < 2.0));
	float p0 = (x0 + x1) - x2;
	float p1 = (x1 + x2) - x0;
	float p2 = (x2 + x0) - x1;
	return (p0 + p1) + p2;
}
