#include <fenv.h>
#include <math.h>
#include <stdint.h>
#define TRUE 1
#define FALSE 0

void __PRECOND(int expr);

double doppler1(double u, double v, double T) {
    __PRECOND((-100.0 <= u) && (u <= 100.0) && (20.0 <= v) && (v <= 20000.0) && (-30.0 <= T) && (T <= 50.0));
	double t1 = 331.4 + (0.6 * T);
	return (-t1 * v) / ((t1 + u) * (t1 + u));
}

double doppler2(double u, double v, double T) {
    __PRECOND((-125.0 <= u) && (u <= 125.0) && (15.0 <= v) && (v <= 25000.0) && (-40.0 <= T) && (T <= 60.0));
	double t1 = 331.4 + (0.6 * T);
	return (-t1 * v) / ((t1 + u) * (t1 + u));
}

double doppler3(double u, double v, double T) {
    __PRECOND((-30.0 <= u) && (u <= 120.0) && (320.0 <= v) && (v <= 20300.0) && (-50.0 <= T) && (T <= 30.0));
	double t1 = 331.4 + (0.6 * T);
	return (-t1 * v) / ((t1 + u) * (t1 + u));
}

double rigidBody1(double x1, double x2, double x3) {
    __PRECOND((-15.0 <= x1) && (x1 <= 15.0) && (-15.0 <= x2) && (x2 <= 15.0) && (-15.0 <= x3) && (x3 <= 15.0));
	return ((-(x1 * x2) - ((2.0 * x2) * x3)) - x1) - x3;
}

double rigidBody2(double x1, double x2, double x3) {
    __PRECOND((-15.0 <= x1) && (x1 <= 15.0) && (-15.0 <= x2) && (x2 <= 15.0) && (-15.0 <= x3) && (x3 <= 15.0));
	return ((((((2.0 * x1) * x2) * x3) + ((3.0 * x3) * x3)) - (((x2 * x1) * x2) * x3)) + ((3.0 * x3) * x3)) - x2;
}

double jetEngineModified(double x1, double x2) {
    __PRECOND((0.0 <= x1) && (x1 <= 5.0) && (-20.0 <= x2) && (x2 <= 5.0));
	double t = (((3.0 * x1) * x1) + (2.0 * x2)) - x1;
	double t_42_ = (((3.0 * x1) * x1) - (2.0 * x2)) - x1;
	double d = (x1 * x1) + 1.0;
	double s = t / d;
	double s_42_ = t_42_ / d;
	return x1 + (((((((((2.0 * x1) * s) * (s - 3.0)) + ((x1 * x1) * ((4.0 * s) - 6.0))) * d) + (((3.0 * x1) * x1) * s)) + ((x1 * x1) * x1)) + x1) + (3.0 * s_42_));
}

double turbine1(double v, double w, double r) {
    __PRECOND((-4.5 <= v) && (v <= -0.3) && (0.4 <= w) && (w <= 0.9) && (3.8 <= r) && (r <= 7.8));
	return ((3.0 + (2.0 / (r * r))) - (((0.125 * (3.0 - (2.0 * v))) * (((w * w) * r) * r)) / (1.0 - v))) - 4.5;
}

double turbine2(double v, double w, double r) {
    __PRECOND((-4.5 <= v) && (v <= -0.3) && (0.4 <= w) && (w <= 0.9) && (3.8 <= r) && (r <= 7.8));
	return ((6.0 * v) - (((0.5 * v) * (((w * w) * r) * r)) / (1.0 - v))) - 2.5;
}

double turbine3(double v, double w, double r) {
    __PRECOND((-4.5 <= v) && (v <= -0.3) && (0.4 <= w) && (w <= 0.9) && (3.8 <= r) && (r <= 7.8));
	return ((3.0 - (2.0 / (r * r))) - (((0.125 * (1.0 + (2.0 * v))) * (((w * w) * r) * r)) / (1.0 - v))) - 0.5;
}

double verhulst(double x) {
    __PRECOND((0.1 <= x) && (x <= 0.3));
	double r = 4.0;
	double K = 1.11;
	return (r * x) / (1.0 + (x / K));
}

double predatorPrey(double x) {
	__PRECOND((0.1 <= x) && (x <= 0.3));
	double r = 4.0;
	double K = 1.11;
	return ((r * x) * x) / (1.0 + ((x / K) * (x / K)));
}

double carbonGas(double v) {
	__PRECOND((0.1 <= v) && (v <= 0.5));
	double p = 35000000.0;
	double a = 0.401;
	double b = 4.27e-5;
	double t = 300.0;
	double n = 1000.0;
	double k = 1.3806503e-23;
	return ((p + ((a * (n / v)) * (n / v))) * (v - (n * b))) - ((k * n) * t);
}

double sine(double x) {
	__PRECOND((-1.57079632679 < x) && (x < 1.57079632679));
	return ((x - (((x * x) * x) / 6.0)) + (((((x * x) * x) * x) * x) / 120.0)) - (((((((x * x) * x) * x) * x) * x) * x) / 5040.0);
}

double sqroot(double x) {
    __PRECOND((0.0 <= x) && (x <= 1.0));
	return (((1.0 + (0.5 * x)) - ((0.125 * x) * x)) + (((0.0625 * x) * x) * x)) - ((((0.0390625 * x) * x) * x) * x);
}

double sineOrder3(double x) {
	__PRECOND((-2.0 < x) && (x < 2.0));
	return (0.954929658551372 * x) - (0.12900613773279798 * ((x * x) * x));
}

// double ex15(double c) {
// 	double a = 3.0;
// 	double b = 3.5;
// 	double discr = (b * b) - ((a * c) * 4.0);
// 	double tmp_1;
// 	if (((b * b) - (a * c)) > 10.0) {
// 		double tmp_2;
// 		if (b > 0.0) {
// 			tmp_2 = (c * 2.0) / (-b - sqrt(discr));
// 		} else if (b < 0.0) {
// 			tmp_2 = (-b + sqrt(discr)) / (a * 2.0);
// 		} else {
// 			tmp_2 = (-b + sqrt(discr)) / (a * 2.0);
// 		}
// 		tmp_1 = tmp_2;
// 	} else {
// 		tmp_1 = (-b + sqrt(discr)) / (a * 2.0);
// 	}
// 	return tmp_1;
// }

// double cav10(double x) { //reassignment in daisy doesn't work, return both cases, ternary 
// 	__PRECOND((0.0 < x) && (x < 10.0));
// 	double tmp;
// 	if (((x * x) - x) >= 0.0) {
// 		tmp = x / 10.0;
// 	} else {
// 		tmp = (x * x) + 2.0;
// 	}
// 	return tmp;
// }

double cav10(double x) {
	__PRECOND((0.0 < x) && (x < 10.0));
	double tmp = (((x * x) - x) >= 0.0) ? x / 10.0 : (x * x) + 2.0;
	return tmp;
}

// double squareRoot3(double x) {
//     __PRECOND((0.0 < x) && (x < 10.0));
// 	double tmp;
// 	if (x < 1e-5) {
// 		tmp = 1.0 + (0.5 * x);
// 	} else {
// 		tmp = sqrt((1.0 + x));
// 	}
// 	return tmp;
// }

// double squareRoot3Invalid(double x) {
//     __PRECOND((0.0 < x) && (x < 10.0));
// 	double tmp;
// 	if (x < 0.0001) {
// 		tmp = 1.0 + (0.5 * x);
// 	} else {
// 		tmp = sqrt((1.0 + x));
// 	}
// 	return tmp;
// }

// double triangle(double a, doudouble triangle(double a, double b, double c) {
//     __PRECOND((1.0 <= a) && (a <= 9.0) && (4.71 <= b) && (b <= 4.89) && (4.71 <= c) && (c <= 4.89));
// 	double s = ((a + b) + c) / 2.0;
// 	return sqrt((((s * (s - a)) * (s - b)) * (s - c)));
// }

// double triangle1(double a, double b, double c) {
//     __PRECOND((1.0 <= a) && (a <= 9.0) && (1.0 <= b) && (b <= 9.0) && (1.0 <= c) && (c <= 9.0) && ((a + b) > (c + 0.1)) && ((a + c) > (b + 0.1)) && ((b + c) > (a + 0.1)));
// 	double s = ((a + b) + c) / 2.0;
// 	return sqrt((((s * (s - a)) * (s - b)) * (s - c)));
// }

// double triangle2(double a, double b, double c) {
//     __PRECOND((1.0 <= a) && (a <= 9.0) && (1.0 <= b) && (b <= 9.0) && (1.0 <= c) && (c <= 9.0) && ((a + b) > (c + 0.01)) && ((a + c) > (b + 0.01)) && ((b + c) > (a + 0.01)));
// 	double s = ((a + b) + c) / 2.0;
// 	return sqrt((((s * (s - a)) * (s - b)) * (s - c)));
// }

// double triangle3(double a, double b, double c) {
//     __PRECOND((1.0 <= a) && (a <= 9.0) && (1.0 <= b) && (b <= 9.0) && (1.0 <= c) && (c <= 9.0) && ((a + b) > (c + 0.001)) && ((a + c) > (b + 0.001)) && ((b + c) > (a + 0.001)));
// 	double s = ((a + b) + c) / 2.0;
// 	return sqrt((((s * (s - a)) * (s - b)) * (s - c)));
// }

// double triangle4(double a, double b, double c) {
//     __PRECOND((1.0 <= a) && (a <= 9.0) && (1.0 <= b) && (b <= 9.0) && (1.0 <= c) && (c <= 9.0) && ((a + b) > (c + 0.0001)) && ((a + c) > (b + 0.0001)) && ((b + c) > (a + 0.0001)));
// 	double s = ((a + b) + c) / 2.0;
// 	return sqrt((((s * (s - a)) * (s - b)) * (s - c)));
// }

// double triangle5(double a, double b, double c) {
//     __PRECOND((1.0 <= a) && (a <= 9.0) && (1.0 <= b) && (b <= 9.0) && (1.0 <= c) && (c <= 9.0) && ((a + b) > (c + 1.0e-05)) && ((a + c) > (b + 1.0e-05)) && ((b + c) > (a + 1.0e-05)));
// 	double s = ((a + b) + c) / 2.0;
// 	return sqrt((((s * (s - a)) * (s - b)) * (s - c)));
// }

// double triangle6(double a, double b, double c) {
//     __PRECOND((1.0 <= a) && (a <= 9.0) && (1.0 <= b) && (b <= 9.0) && (1.0 <= c) && (c <= 9.0) && ((a + b) > (c + 1.0e-06)) && ((a + c) > (b + 1.0e-06)) && ((b + c) > (a + 1.0e-06)));
// 	double s = ((a + b) + c) / 2.0;
// 	return sqrt((((s * (s - a)) * (s - b)) * (s - c)));
// }

// double triangle7(double a, double b, double c) {
//     __PRECOND((1.0 <= a) && (a <= 9.0) && (1.0 <= b) && (b <= 9.0) && (1.0 <= c) && (c <= 9.0) && ((a + b) > (c + 1.0e-07)) && ((a + c) > (b + 1.0e-07)) && ((b + c) > (a + 1.0e-07)));
// 	double s = ((a + b) + c) / 2.0;
// 	return sqrt((((s * (s - a)) * (s - b)) * (s - c)));
// }

// double triangle8(double a, double b, double c) {
// 	double s = ((a + b) + c) / 2.0;
// 	return sqrt((((s * (s - a)) * (s - b)) * (s - c)));
// }

// double triangle9(double a, double b, double c) {
//     __PRECOND((1.0 <= a) && (a <= 9.0) && (1.0 <= b) && (b <= 9.0) && (1.0 <= c) && (c <= 9.0) && ((a + b) > (c + 1.0e-09)) && ((a + c) > (b + 1.0e-09)) && ((b + c) > (a + 1.0e-09)));
// 	double s = ((a + b) + c) / 2.0;
// 	return sqrt((((s * (s - a)) * (s - b)) * (s - c)));
// }

// double triangle10(double a, double b, double c) {
//     __PRECOND((1.0 <= a) && (a <= 9.0) && (1.0 <= b) && (b <= 9.0) && (1.0 <= c) && (c <= 9.0) && ((a + b) > (c + 1.0e-10)) && ((a + c) > (b + 1.0e-10)) && ((b + c) > (a + 1.0e-10)));
// 	double s = ((a + b) + c) / 2.0;
// 	return sqrt((((s * (s - a)) * (s - b)) * (s - c)));
// }

// double triangle11(double a, double b, double c) {
//     __PRECOND((1.0 <= a) && (a <= 9.0) && (1.0 <= b) && (b <= 9.0) && (1.0 <= c) && (c <= 9.0) && ((a + b) > (c + 1.0e-11)) && ((a + c) > (b + 1.0e-11)) && ((b + c) > (a + 1.0e-11)));
// 	double s = ((a + b) + c) / 2.0;
// 	return sqrt((((s * (s - a)) * (s - b)) * (s - c)));
// }

// double triangle12(double a, double b, double c) {
//     __PRECOND((1.0 <= a) && (a <= 9.0) && (1.0 <= b) && (b <= 9.0) && (1.0 <= c) && (c <= 9.0) && ((a + b) > (c + 1.0e-12)) && ((a + c) > (b + 1.0e-12)) && ((b + c) > (a + 1.0e-12)));
// 	double s = ((a + b) + c) / 2.0;
// 	return sqrt((((s * (s - a)) * (s - b)) * (s - c)));
// }

// double bspline3(double u) {
// 	__PRECOND((0.0 <= u) && (u <= 1.0));
// 	return -((u * u) * u) / 6.0;
// }

// double triangleSorted(double a, double b, double c) {
//     __PRECOND((1.0 <= a) && (a <= 9.0) && (1.0 <= b) && (b <= 9.0) && (1.0 <= c) && (c <= 9.0) && ((a + b) > (c + 1.0e-06)) && ((a + c) > (b + 1.0e-06)) && ((b + c) > (a + 1.0e-06)) && (a < c) && (b < c));
// 	double tmp;
// 	if (a < b) {
// 		tmp = sqrt(((((c + (b + a)) * (a - (c - b))) * (a + (c - b))) * (c + (b - a)))) / 4.0;
// 	} else {
// 		tmp = sqrt(((((c + (a + b)) * (b - (c - a))) * (b + (c - a))) * (c + (a - b)))) / 4.0;
// 	}
// 	return tmp;
// }ble b, double c) {
//     __PRECOND((1.0 <= a) && (a <= 9.0) && (4.71 <= b) && (b <= 4.89) && (4.71 <= c) && (c <= 4.89));
// 	double s = ((a + b) + c) / 2.0;
// 	return sqrt((((s * (s - a)) * (s - b)) * (s - c)));
// }

// double triangle1(double a, double b, double c) {
//     __PRECOND((1.0 <= a) && (a <= 9.0) && (1.0 <= b) && (b <= 9.0) && (1.0 <= c) && (c <= 9.0) && ((a + b) > (c + 0.1)) && ((a + c) > (b + 0.1)) && ((b + c) > (a + 0.1)));
// 	double s = ((a + b) + c) / 2.0;
// 	return sqrt((((s * (s - a)) * (s - b)) * (s - c)));
// }

// double triangle2(double a, double b, double c) {
//     __PRECOND((1.0 <= a) && (a <= 9.0) && (1.0 <= b) && (b <= 9.0) && (1.0 <= c) && (c <= 9.0) && ((a + b) > (c + 0.01)) && ((a + c) > (b + 0.01)) && ((b + c) > (a + 0.01)));
// 	double s = ((a + b) + c) / 2.0;
// 	return sqrt((((s * (s - a)) * (s - b)) * (s - c)));
// }

// double triangle3(double a, double b, double c) {
//     __PRECOND((1.0 <= a) && (a <= 9.0) && (1.0 <= b) && (b <= 9.0) && (1.0 <= c) && (c <= 9.0) && ((a + b) > (c + 0.001)) && ((a + c) > (b + 0.001)) && ((b + c) > (a + 0.001)));
// 	double s = ((a + b) + c) / 2.0;
// 	return sqrt((((s * (s - a)) * (s - b)) * (s - c)));
// }

// double triangle4(double a, double b, double c) {
//     __PRECOND((1.0 <= a) && (a <= 9.0) && (1.0 <= b) && (b <= 9.0) && (1.0 <= c) && (c <= 9.0) && ((a + b) > (c + 0.0001)) && ((a + c) > (b + 0.0001)) && ((b + c) > (a + 0.0001)));
// 	double s = ((a + b) + c) / 2.0;
// 	return sqrt((((s * (s - a)) * (s - b)) * (s - c)));
// }

// double triangle5(double a, double b, double c) {
//     __PRECOND((1.0 <= a) && (a <= 9.0) && (1.0 <= b) && (b <= 9.0) && (1.0 <= c) && (c <= 9.0) && ((a + b) > (c + 1.0e-05)) && ((a + c) > (b + 1.0e-05)) && ((b + c) > (a + 1.0e-05)));
// 	double s = ((a + b) + c) / 2.0;
// 	return sqrt((((s * (s - a)) * (s - b)) * (s - c)));
// }

// double triangle6(double a, double b, double c) {
//     __PRECOND((1.0 <= a) && (a <= 9.0) && (1.0 <= b) && (b <= 9.0) && (1.0 <= c) && (c <= 9.0) && ((a + b) > (c + 1.0e-06)) && ((a + c) > (b + 1.0e-06)) && ((b + c) > (a + 1.0e-06)));
// 	double s = ((a + b) + c) / 2.0;
// 	return sqrt((((s * (s - a)) * (s - b)) * (s - c)));
// }

// double triangle7(double a, double b, double c) {
//     __PRECOND((1.0 <= a) && (a <= 9.0) && (1.0 <= b) && (b <= 9.0) && (1.0 <= c) && (c <= 9.0) && ((a + b) > (c + 1.0e-07)) && ((a + c) > (b + 1.0e-07)) && ((b + c) > (a + 1.0e-07)));
// 	double s = ((a + b) + c) / 2.0;
// 	return sqrt((((s * (s - a)) * (s - b)) * (s - c)));
// }

// double triangle8(double a, double b, double c) {
// 	double s = ((a + b) + c) / 2.0;
// 	return sqrt((((s * (s - a)) * (s - b)) * (s - c)));
// }

// double triangle9(double a, double b, double c) {
//     __PRECOND((1.0 <= a) && (a <= 9.0) && (1.0 <= b) && (b <= 9.0) && (1.0 <= c) && (c <= 9.0) && ((a + b) > (c + 1.0e-09)) && ((a + c) > (b + 1.0e-09)) && ((b + c) > (a + 1.0e-09)));
// 	double s = ((a + b) + c) / 2.0;
// 	return sqrt((((s * (s - a)) * (s - b)) * (s - c)));
// }

// double triangle10(double a, double b, double c) {
//     __PRECOND((1.0 <= a) && (a <= 9.0) && (1.0 <= b) && (b <= 9.0) && (1.0 <= c) && (c <= 9.0) && ((a + b) > (c + 1.0e-10)) && ((a + c) > (b + 1.0e-10)) && ((b + c) > (a + 1.0e-10)));
// 	double s = ((a + b) + c) / 2.0;
// 	return sqrt((((s * (s - a)) * (s - b)) * (s - c)));
// }

// double triangle11(double a, double b, double c) {
//     __PRECOND((1.0 <= a) && (a <= 9.0) && (1.0 <= b) && (b <= 9.0) && (1.0 <= c) && (c <= 9.0) && ((a + b) > (c + 1.0e-11)) && ((a + c) > (b + 1.0e-11)) && ((b + c) > (a + 1.0e-11)));
// 	double s = ((a + b) + c) / 2.0;
// 	return sqrt((((s * (s - a)) * (s - b)) * (s - c)));
// }

// double triangle12(double a, double b, double c) {
//     __PRECOND((1.0 <= a) && (a <= 9.0) && (1.0 <= b) && (b <= 9.0) && (1.0 <= c) && (c <= 9.0) && ((a + b) > (c + 1.0e-12)) && ((a + c) > (b + 1.0e-12)) && ((b + c) > (a + 1.0e-12)));
// 	double s = ((a + b) + c) / 2.0;
// 	return sqrt((((s * (s - a)) * (s - b)) * (s - c)));
// }

double bspline3(double u) {
	__PRECOND((0.0 <= u) && (u <= 1.0));
	return -((u * u) * u) / 6.0;
}

// double triangleSorted(double a, double b, double c) {
//     __PRECOND((1.0 <= a) && (a <= 9.0) && (1.0 <= b) && (b <= 9.0) && (1.0 <= c) && (c <= 9.0) && ((a + b) > (c + 1.0e-06)) && ((a + c) > (b + 1.0e-06)) && ((b + c) > (a + 1.0e-06)) && (a < c) && (b < c));
// 	double tmp;
// 	if (a < b) {
// 		tmp = sqrt(((((c + (b + a)) * (a - (c - b))) * (a + (c - b))) * (c + (b - a)))) / 4.0;
// 	} else {
// 		tmp = sqrt(((((c + (a + b)) * (b - (c - a))) * (b + (c - a))) * (c + (a - b)))) / 4.0;
// 	}
// 	return tmp;
// }

// double ex34(double x0, double y0, double z0, double vx0, double vy0, double vz0) {
// 	double dt = 0.1;
// 	double solarMass = 39.47841760435743;
// 	double x = x0;
// 	double y = y0;
// 	double z = z0;
// 	double vx = vx0;
// 	double vy = vy0;
// 	double vz = vz0;
// 	double i = 0.0;
// 	int tmp = i < 100.0;
// 	while (tmp) {
// 		double distance = sqrt((((x * x) + (y * y)) + (z * z)));
// 		double mag = dt / ((distance * distance) * distance);
// 		double vxNew = vx - ((x * solarMass) * mag);
// 		double x_1 = x + (dt * vxNew);
// 		double distance_2 = sqrt((((x * x) + (y * y)) + (z * z)));
// 		double mag_3 = dt / ((distance_2 * distance_2) * distance_2);
// 		double vyNew = vy - ((y * solarMass) * mag_3);
// 		double y_4 = y + (dt * vyNew);
// 		double distance_5 = sqrt((((x * x) + (y * y)) + (z * z)));
// 		double mag_6 = dt / ((distance_5 * distance_5) * distance_5);
// 		double vzNew = vz - ((z * solarMass) * mag_6);
// 		double z_7 = z + (dt * vzNew);
// 		double distance_8 = sqrt((((x * x) + (y * y)) + (z * z)));
// 		double mag_9 = dt / ((distance_8 * distance_8) * distance_8);
// 		double vx_10 = vx - ((x * solarMass) * mag_9);
// 		double distance_11 = sqrt((((x * x) + (y * y)) + (z * z)));
// 		double mag_12 = dt / ((distance_11 * distance_11) * distance_11);
// 		double vy_13 = vy - ((y * solarMass) * mag_12);
// 		double distance_14 = sqrt((((x * x) + (y * y)) + (z * z)));
// 		double mag_15 = dt / ((distance_14 * distance_14) * distance_14);
// 		double vz_16 = vz - ((z * solarMass) * mag_15);
// 		double i_17 = i + 1.0;
// 		x = x_1;
// 		y = y_4;
// 		z = z_7;
// 		vx = vx_10;
// 		vy = vy_13;
// 		vz = vz_16;
// 		i = i_17;
// 		tmp = i < 100.0;
// 	}
// 	return x;
// }

// double ex35(double t0, double w0, double N) {
// 	double h = 0.01;
// 	double L = 2.0;
// 	double m = 1.5;
// 	double g = 9.80665;
// 	double t = t0;
// 	double w = w0;
// 	double n = 0.0;
// 	int tmp = n < N;
// 	while (tmp) {
// 		double k1w = (-g / L) * sin(t);
// 		double k2t = w + ((h / 2.0) * k1w);
// 		double t_1 = t + (h * k2t);
// 		double k2w = (-g / L) * sin((t + ((h / 2.0) * w)));
// 		double w_2 = w + (h * k2w);
// 		double n_3 = n + 1.0;
// 		t = t_1;
// 		w = w_2;
// 		n = n_3;
// 		tmp = n < N;
// 	}
// 	return t;
// }

// double ex36(double x0) {
// 	double x = x0;
// 	double i = 0.0;
// 	int tmp = i < 10.0;
// 	while (tmp) {
// 		double x_1 = x - ((((x - (pow(x, 3.0) / 6.0)) + (pow(x, 5.0) / 120.0)) + (pow(x, 7.0) / 5040.0)) / (((1.0 - ((x * x) / 2.0)) + (pow(x, 4.0) / 24.0)) + (pow(x, 6.0) / 720.0)));
// 		double i_2 = i + 1.0;
// 		x = x_1;
// 		i = i_2;
// 		tmp = i < 10.0;
// 	}
// 	return x;
// }

