#include <fenv.h>
#include <math.h>
#include <stdint.h>
#define TRUE 1
#define FALSE 0

void __PRECOND(int expr);

double hartman3(double x1, double x2, double x3) {
    __PRECOND((0.0 <= x1) && (x1 <= 1.0) && (0.0 <= x2) && (x2 <= 1.0) && (0.0 <= x3) && (x3 <= 1.0));
	double e1 = ((3.0 * ((x1 - 0.3689) * (x1 - 0.3689))) + (10.0 * ((x2 - 0.117) * (x2 - 0.117)))) + (30.0 * ((x3 - 0.2673) * (x3 - 0.2673)));
	double e2 = ((0.1 * ((x1 - 0.4699) * (x1 - 0.4699))) + (10.0 * ((x2 - 0.4387) * (x2 - 0.4387)))) + (35.0 * ((x3 - 0.747) * (x3 - 0.747)));
	double e3 = ((3.0 * ((x1 - 0.1091) * (x1 - 0.1091))) + (10.0 * ((x2 - 0.8732) * (x2 - 0.8732)))) + (30.0 * ((x3 - 0.5547) * (x3 - 0.5547)));
	double e4 = ((0.1 * ((x1 - 0.03815) * (x1 - 0.03815))) + (10.0 * ((x2 - 0.5743) * (x2 - 0.5743)))) + (35.0 * ((x3 - 0.8828) * (x3 - 0.8828)));
	double exp1 = exp(-e1);
	double exp2 = exp(-e2);
	double exp3 = exp(-e3);
	double exp4 = exp(-e4);
	return -((((1.0 * exp1) + (1.2 * exp2)) + (3.0 * exp3)) + (3.2 * exp4));
}
