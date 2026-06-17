#include <fenv.h>
#include <math.h>
#include <stdint.h>
#define TRUE 1
#define FALSE 0

void __PRECOND(int expr);

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
