#include <fenv.h>
#include <math.h>
#include <stdint.h>
#define TRUE 1
#define FALSE 0

void __PRECOND(int expr);

// float ex0(float t) {
// 	float tmp_2 = t + 1.0f;
// 	double tmp_1 = ((double) t) / ((double) tmp_2);
// 	return (float) tmp_1;
// }

double delta4(double x1, double x2, double x3, double x4, double x5, double x6) {
	__PRECOND((4.0 <= x1) && (x1 <= 6.3504) && (4.0 <= x2) && (x2 <= 6.3504) && (4.0 <= x3) && (x3 <= 6.3504) && (4.0 <= x4) && (x4 <= 6.3504) && (4.0 <= x5) && (x5 <= 6.3504) && (4.0 <= x6) && (x6 <= 6.3504));
	return (((((-x2 * x3) - (x1 * x4)) + (x2 * x5)) + (x3 * x6)) - (x5 * x6)) + (x1 * (((((-x1 + x2) + x3) - x4) + x5) + x6));
}

double delta(double x1, double x2, double x3, double x4, double x5, double x6) {
    __PRECOND((4.0 <= x1) && (x1 <= 6.3504) && (4.0 <= x2) && (x2 <= 6.3504) && (4.0 <= x3) && (x3 <= 6.3504) && (4.0 <= x4) && (x4 <= 6.3504) && (4.0 <= x5) && (x5 <= 6.3504) && (4.0 <= x6) && (x6 <= 6.3504));
	return (((((((x1 * x4) * (((((-x1 + x2) + x3) - x4) + x5) + x6)) + ((x2 * x5) * (((((x1 - x2) + x3) + x4) - x5) + x6))) + ((x3 * x6) * (((((x1 + x2) - x3) + x4) + x5) - x6))) + ((-x2 * x3) * x4)) + ((-x1 * x3) * x5)) + ((-x1 * x2) * x6)) + ((-x4 * x5) * x6);
}

double sqrt_add(double x) {
    __PRECOND((1.0 <= x) && (x <= 1000.0));
	return 1.0 / (sqrt((x + 1.0)) + sqrt(x));
}

double exp1x(double x) {
    __PRECOND((0.01 <= x) && (x <= 0.5));
	return (exp(x) - 1.0) / x;
}

float exp1x_32(float x) {
    __PRECOND((0.01 <= x) && (x <= 0.5));
	return (expf(x) - 1.0f) / x;
}

double floudas(double x1, double x2) {
    __PRECOND((0.0 <= x1) && (x1 <= 2.0) && (0.0 <= x2) && (x2 <= 3.0) && ((x1 + x2) <= 2.0));
	return x1 + x2;
}

double exp1x_log(double x) {
    __PRECOND((0.01 <= x) && (x <= 0.5));
	return (exp(x) - 1.0) / log(exp(x));
}

float x_by_xy(float x, float y) {
	__PRECOND((1.0 <= x) && (x <= 4.0) && (1.0 <= y) && (y <= 4.0));
	return x / (x + y);
}

double hypot(double x1, double x2) {
    __PRECOND((1.0 <= x1) && (x1 <= 100.0) && (1.0 <= x2) && (x2 <= 100.0));
	return sqrt(((x1 * x1) + (x2 * x2)));
}

float hypot32(float x1, float x2) {
    __PRECOND((1.0 <= x1) && (x1 <= 100.0) && (1.0 <= x2) && (x2 <= 100.0));
	return sqrtf(((x1 * x1) + (x2 * x2)));
}

// can't find the equivalent scala file
// double ex11(double x) {
// 	return log((1.0 + exp(x)));
// }

double sum(double x0, double x1, double x2) {
	__PRECOND((1.0 <= x0) && (x0 <= 2.0) && (1.0 <= x1) && (x1 <= 2.0) && (1.0 <= x2) && (x2 <= 2.0));
	double p0 = (x0 + x1) - x2;
	double p1 = (x1 + x2) - x0;
	double p2 = (x2 + x0) - x1;
	return (p0 + p1) + p2;
}

double nonlin1(double z) {
	__PRECOND((0.0 <= z) && (z <= 999.0));
	return z / (z + 1.0);
}

double nonlin2(double x, double y) {
	__PRECOND((1.001 <= x) && (x <= 2.0) && (1.001 <= y) && (y <= 2.0));
	double t = x * y;
	return (t - 1.0) / ((t * t) - 1.0);
}

// float i4(float x, float y) {
//     __PRECOND((0.1 <= x) && (x <= 10.0) && (-5.0 <= y) && (y <= 5.0));
// 	return sqrtf((x + (y * y)));
// }

float i6(float x, float y) {
    __PRECOND((0.1 <= x) && (x <= 10.0) && (-5.0 <= y) && (y <= 5.0));
	return sinf((x * y));
}

double himmilbeau(double x1, double x2) {
    __PRECOND((-5.0 <= x1) && (x1 <= 5.0) && (-5.0 <= x2) && (x2 <= 5.0));
	double a = ((x1 * x1) + x2) - 11.0;
	double b = (x1 + (x2 * x2)) - 7.0;
	return (a * a) + (b * b);
}

