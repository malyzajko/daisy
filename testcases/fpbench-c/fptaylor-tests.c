#include <fenv.h>
#include <math.h>
#include <stdint.h>
#define TRUE 1
#define FALSE 0

void __PRECOND(int expr);

double intro_example(double t) {
    __PRECOND((0.0 <= t) && (t <= 999.0));
	return t / (t + 1.0);
}

double sec4_example(double x, double y) {
    __PRECOND((1.001 <= x) && (x <= 2.0) && (1.001 <= y) && (y <= 2.0));
	double t = x * y;
	return (t - 1.0) / ((t * t) - 1.0);
}

float test01_sum3(float x0, float x1, float x2) {
    __PRECOND((1.0 < x0) && (x0 < 2.0) && (1.0 < x1) && (x1 < 2.0) && (1.0 < x2) && (x2 < 2.0));
	float p0 = (x0 + x1) - x2;
	float p1 = (x1 + x2) - x0;
	float p2 = (x2 + x0) - x1;
	return (p0 + p1) + p2;
}

double test02_sum8(double x0, double x1, double x2, double x3, double x4, double x5, double x6, double x7) {
    __PRECOND((1.0 < x0) && (x0 < 2.0) && (1.0 < x1) && (x1 < 2.0) && (1.0 < x2) && (x2 < 2.0) && (1.0 < x3) && (x3 < 2.0) && (1.0 < x4) && (x4 < 2.0) && (1.0 < x5) && (x5 < 2.0) && (1.0 < x6) && (x6 < 2.0) && (1.0 < x7) && (x7 < 2.0));
	return ((((((x0 + x1) + x2) + x3) + x4) + x5) + x6) + x7;
}

double test03_nonlin2(double x, double y) {
    __PRECOND((0.0 < x) && (x < 1.0) && (-1.0 < y) && (y < -0.1));
	return (x + y) / (x - y);
}

double test04_dqmom9(double m0, double m1, double m2, double w0, double w1, double w2, double a0, double a1, double a2) {
    __PRECOND((-1.0 < m0) && (m0 < 1.0) && (-1.0 < m1) && (m1 < 1.0) && (-1.0 < m2) && (m2 < 1.0) && (1.0e-05 < w0) && (w0 < 1.0) && (1.0e-05 < w1) && (w1 < 1.0) && (1.0e-05 < w2) && (w2 < 1.0) && (1.0e-05 < a0) && (a0 < 1.0) && (1.0e-05 < a1) && (a1 < 1.0) && (1.0e-05 < a2) && (a2 < 1.0));
	double v2 = (w2 * (0.0 - m2)) * (-3.0 * ((1.0 * (a2 / w2)) * (a2 / w2)));
	double v1 = (w1 * (0.0 - m1)) * (-3.0 * ((1.0 * (a1 / w1)) * (a1 / w1)));
	double v0 = (w0 * (0.0 - m0)) * (-3.0 * ((1.0 * (a0 / w0)) * (a0 / w0)));
	return 0.0 + ((v0 * 1.0) + ((v1 * 1.0) + ((v2 * 1.0) + 0.0)));
}

double test05_nonlin1_r4(double x) {
    __PRECOND((1.00001 < x) && (x < 2.0));
	double r1 = x - 1.0;
	double r2 = x * x;
	return r1 / (r2 - 1.0);
}

double test05_nonlin1_test2(double x) {
    __PRECOND((1.00001 < x) && (x < 2.0));
	return 1.0 / (x + 1.0);
}

float test06_sums4_sum1(float x0, float x1, float x2, float x3) {
    __PRECOND((-1.0e-05 < x0) && (x0 < 1.00001) && (0.0 < x1) && (x1 < 1.0) && (0.0 < x2) && (x2 < 1.0) && (0.0 < x3) && (x3 < 1.0));
	return ((x0 + x1) + x2) + x3;
}

float test06_sums4_sum2(float x0, float x1, float x2, float x3) {
    __PRECOND((-1.0e-05 < x0) && (x0 < 1.00001) && (0.0 < x1) && (x1 < 1.0) && (0.0 < x2) && (x2 < 1.0) && (0.0 < x3) && (x3 < 1.0));
	return (x0 + x1) + (x2 + x3);
}

