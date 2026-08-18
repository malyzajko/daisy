#include <fenv.h>
#include <math.h>
#include <stdint.h>
#define TRUE 1
#define FALSE 0

void __PRECOND(int expr);

double carthesianToPolar_radius(double x, double y) {
	__PRECOND((1.0 <= x) && (x <= 100.0) && (1.0 <= y) && (y <= 100.0));
	return sqrt(((x * x) + (y * y)));
}

double carthesianToPolar_theta(double x, double y) {
	__PRECOND((1.0 <= x) && (x <= 100.0) && (1.0 <= y) && (y <= 100.0));
	double pi = 3.14159265359;
	double radiant = atan((y / x));
	return radiant * (180.0 / pi);
}

double polarToCarthesian_x(double radius, double theta) {
	__PRECOND((1.0 <= radius) && (radius <= 10.0) && (0.0 <= theta) && (theta <= 360.0));
	double pi = 3.14159265359;
	double radiant = theta * (pi / 180.0);
	return radius * cos(radiant);
}

double polarToCarthesian_y(double radius, double theta) {
	__PRECOND((1.0 <= radius) && (radius <= 10.0) && (0.0 <= theta) && (theta <= 360.0));
	double pi = 3.14159265359;
	double radiant = theta * (pi / 180.0);
	return radius * sin(radiant);
}

double instantaneousCurrent(double t, double resistance, double frequency, double inductance, double maxVoltage) {
	__PRECOND((0.0 <= t) && (t <= 300.0) && (1.0 <= resistance) && (resistance <= 50.0) && (1.0 <= frequency) && (frequency <= 100.0) && (0.001 <= inductance) && (inductance <= 0.004) && (1.0 <= maxVoltage) && (maxVoltage <= 12.0));
	double pi = 3.14159265359;
	double impedance_re = resistance;
	double impedance_im = ((2.0 * pi) * frequency) * inductance;
	double denom = (impedance_re * impedance_re) + (impedance_im * impedance_im);
	double current_re = (maxVoltage * impedance_re) / denom;
	double current_im = -(maxVoltage * impedance_im) / denom;
	double maxCurrent = sqrt(((current_re * current_re) + (current_im * current_im)));
	double theta = atan((current_im / current_re));
	return maxCurrent * cos(((((2.0 * pi) * frequency) * t) + theta));
}

double matrixDeterminant(double a, double b, double c, double d, double e, double f, double g, double h, double i) {
	__PRECOND((-10.0 <= a) && (a <= 10.0) && (-10.0 <= b) && (b <= 10.0) && (-10.0 <= c) && (c <= 10.0) && (-10.0 <= d) && (d <= 10.0) && (-10.0 <= e) && (e <= 10.0) && (-10.0 <= f) && (f <= 10.0) && (-10.0 <= g) && (g <= 10.0) && (-10.0 <= h) && (h <= 10.0) && (-10.0 <= i) && (i <= 10.0));
	return ((((a * e) * i) + ((b * f) * g)) + ((c * d) * h)) - ((((c * e) * g) + ((b * d) * i)) + ((a * f) * h));
}

double matrixDeterminant2(double a, double b, double c, double d, double e, double f, double g, double h, double i) {
	__PRECOND((-10.0 <= a) && (a <= 10.0) && (-10.0 <= b) && (b <= 10.0) && (-10.0 <= c) && (c <= 10.0) && (-10.0 <= d) && (d <= 10.0) && (-10.0 <= e) && (e <= 10.0) && (-10.0 <= f) && (f <= 10.0) && (-10.0 <= g) && (g <= 10.0) && (-10.0 <= h) && (h <= 10.0) && (-10.0 <= i) && (i <= 10.0));
	return ((((e * i) * a) + (((b * f) * g) + ((d * h) * c))) - (((c * g) * e) + (((b * d) * i) + ((f * h) * a))));
}

