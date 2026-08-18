#include <fenv.h>
#include <math.h>
#include <stdint.h>
#define TRUE 1
#define FALSE 0

double ex0(int64_t n) {
	double dppi = (double) M_PI;
	double h = dppi / ((double) n);
	double t1 = 0.0;
	double t2 = 0.0;
	double s1 = 0.0;
	double t1_1 = t1;
	int64_t tmp = 1;
	int64_t i = tmp;
	int tmp_2 = i <= n;
	while (tmp_2) {
		double x = ((double) i) * h;
		float tmp_3 = 1.0f;
		float d1 = tmp_3;
		double t1_4 = x;
		int64_t tmp_5 = 1;
		int64_t k = tmp_5;
		int tmp_6 = k <= 5.0;
		while (tmp_6) {
			float tmp_7 = d1 * 2.0f;
			d1 = tmp_7;
			t1_4 = t1_4 + (sin((((double) d1) * x)) / ((double) d1));
			int64_t tmp_8 = k + 1;
			k = tmp_8;
			tmp_6 = k <= 5.0;
		}
		t2 = t1_4;
		double s0 = sqrt(((h * h) + ((t2 - t1_1) * (t2 - t1_1))));
		long double tmp_9 = ((long double) s1) + ((long double) s0);
		s1 = tmp_9;
		t1_1 = t2;
		int64_t tmp_10 = i + 1;
		i = tmp_10;
		tmp_2 = i <= n;
	}
	return s1;
}

double ex1(double n) {
	double dppi = acos(-1.0);
	double h = dppi / n;
	double s1 = 0.0;
	double t1 = 0.0;
	double i = 1.0;
	int tmp = i <= n;
	while (tmp) {
		double x = i * h;
		float tmp_1 = 2.0f;
		float d0 = tmp_1;
		double t0 = x;
		double k = 1.0;
		int tmp_2 = k <= 5.0;
		while (tmp_2) {
			float tmp_3 = 2.0f * d0;
			float d0_4 = tmp_3;
			double t0_5 = t0 + (sin((((double) d0) * x)) / ((double) d0));
			double k_6 = k + 1.0;
			d0 = d0_4;
			t0 = t0_5;
			k = k_6;
			tmp_2 = k <= 5.0;
		}
		double t2 = t0;
		double s0 = sqrt(((h * h) + ((t2 - t1) * (t2 - t1))));
		long double tmp_7 = ((long double) s1) + ((long double) s0);
		long double s1_8 = tmp_7;
		double x_9 = i * h;
		float tmp_10 = 2.0f;
		float d0_11 = tmp_10;
		double t0_12 = x_9;
		double k_13 = 1.0;
		int tmp_14 = k_13 <= 5.0;
		while (tmp_14) {
			float tmp_15 = 2.0f * d0_11;
			float d0_16 = tmp_15;
			double t0_17 = t0_12 + (sin((((double) d0_11) * x_9)) / ((double) d0_11));
			double k_18 = k_13 + 1.0;
			d0_11 = d0_16;
			t0_12 = t0_17;
			k_13 = k_18;
			tmp_14 = k_13 <= 5.0;
		}
		double t2_19 = t0_12;
		double t1_20 = t2_19;
		double i_21 = i + 1.0;
		s1 = s1_8;
		t1 = t1_20;
		i = i_21;
		tmp = i <= n;
	}
	return s1;
}

