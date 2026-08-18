#include <fenv.h>
#include <math.h>
#include <stdint.h>
#define TRUE 1
#define FALSE 0

float ex0(float sr_42_, float sl_42_) {
	float inv_l = 0.1f;
	float c = 12.34f;
	float delta_dl = 0.0f;
	float delta_dr = 0.0f;
	float delta_d = 0.0f;
	float delta_theta = 0.0f;
	float arg = 0.0f;
	float cosi = 0.0f;
	float x = 0.0f;
	float sini = 0.0f;
	float y = 0.0f;
	float theta = -0.985f;
	float t = 0.0f;
	float tmp = sl_42_;
	float sl = sl_42_;
	float sr = sr_42_;
	float j = 0.0f;
	int tmp_1 = t < 1000.0f;
	while (tmp_1) {
		delta_dl = c * sl;
		delta_dr = c * sr;
		delta_d = (delta_dl + delta_dr) * 0.5f;
		delta_theta = (delta_dr - delta_dl) * inv_l;
		arg = theta + (delta_theta * 0.5f);
		cosi = (1.0f - ((arg * arg) * 0.5f)) + ((((arg * arg) * arg) * arg) * 0.0416666666f);
		x = x + (delta_d * cosi);
		sini = (arg - (((arg * arg) * arg) * 0.1666666666f)) + (((((arg * arg) * arg) * arg) * arg) * 0.008333333f);
		y = y + (delta_d * sini);
		theta = theta + delta_theta;
		t = t + 1.0f;
		tmp = sl;
		float tmp_2;
		if (j == 50.0f) {
			tmp_2 = sr;
		} else {
			tmp_2 = sl;
		}
		sl = tmp_2;
		float tmp_3;
		if (j == 50.0f) {
			tmp_3 = tmp;
		} else {
			tmp_3 = sr;
		}
		sr = tmp_3;
		float tmp_4;
		if (j == 50.0f) {
			tmp_4 = 0.0f;
		} else {
			tmp_4 = j + 1.0f;
		}
		j = tmp_4;
		tmp_1 = t < 1000.0f;
	}
	return x;
}

double ex1(double m, double kp, double ki, double kd, double c) {
	double dt = 0.5;
	double invdt = 1.0 / dt;
	double e = 0.0;
	double p = 0.0;
	double i = 0.0;
	double d = 0.0;
	double r = 0.0;
	double m_1 = m;
	double eold = 0.0;
	double t = 0.0;
	int tmp = t < 100.0;
	while (tmp) {
		e = c - m_1;
		p = kp * e;
		i = i + ((ki * dt) * e);
		d = (kd * invdt) * (e - eold);
		r = (p + i) + d;
		m_1 = m_1 + (0.01 * r);
		eold = e;
		t = t + dt;
		tmp = t < 100.0;
	}
	return m_1;
}

float ex2(float h, float y_n_42_, float c) {
	float sixieme = 1.0f / 6.0f;
	float eps = 0.005f;
	float k = 1.2f;
	float y_n = y_n_42_;
	float i = 0.0f;
	float e = 1.0f;
	int tmp = e > eps;
	while (tmp) {
		float v = c - y_n;
		float k1 = (k * v) * v;
		float v_1 = c - (y_n + ((0.5f * h) * k1));
		float k2 = (k * v_1) * v_1;
		float v_2 = c - (y_n + ((0.5f * h) * k2));
		float k3 = (k * v_2) * v_2;
		float v_3 = c - (y_n + (h * k3));
		float k4 = (k * v_3) * v_3;
		float y_n_4 = y_n + ((sixieme * h) * (((k1 + (2.0f * k2)) + (2.0f * k3)) + k4));
		float i_5 = i + 1.0f;
		float e_6 = e - eps;
		y_n = y_n_4;
		i = i_5;
		e = e_6;
		tmp = e > eps;
	}
	return fabsf(e);
}

float ex3(float y, float yd) {
	float eps = 0.01f;
	float Dc = -1280.0f;
	float Ac00 = 0.499f;
	float Ac01 = -0.05f;
	float Ac10 = 0.01f;
	float Ac11 = 1.0f;
	float Bc0 = 1.0f;
	float Bc1 = 0.0f;
	float Cc0 = 564.48f;
	float Cc1 = 0.0f;
	float yc = 0.0f;
	float u = 0.0f;
	float xc0 = 0.0f;
	float xc1 = 0.0f;
	float i = 0.0f;
	float e = 1.0f;
	int tmp = e > eps;
	while (tmp) {
		float v = y - yd;
		float tmp_1;
		if (v < -1.0f) {
			tmp_1 = -1.0f;
		} else if (1.0f < v) {
			tmp_1 = 1.0f;
		} else {
			tmp_1 = v;
		}
		yc = tmp_1;
		u = (Cc0 * xc0) + ((Cc1 * xc1) + (Dc * yc));
		xc0 = (Ac00 * xc0) + ((Ac01 * xc1) + (Bc0 * yc));
		xc1 = (Ac10 * xc0) + ((Ac11 * xc1) + (Bc1 * yc));
		i = i + 1.0f;
		e = fabsf((yc - xc1));
		tmp = e > eps;
	}
	return xc1;
}

double ex4(double u) {
	double a = 0.25;
	double b = 5000.0;
	double n = 25.0;
	double h = (b - a) / n;
	double xb = 0.0;
	double r = 0.0;
	double xa = 0.25;
	int tmp = xa < 5000.0;
	while (tmp) {
		double v = xa + h;
		double tmp_1;
		if (v > 5000.0) {
			tmp_1 = 5000.0;
		} else {
			tmp_1 = v;
		}
		xb = tmp_1;
		double gxa = u / ((((((0.7 * xa) * xa) * xa) - ((0.6 * xa) * xa)) + (0.9 * xa)) - 0.2);
		double gxb = u / ((((((0.7 * xb) * xb) * xb) - ((0.6 * xb) * xb)) + (0.9 * xb)) - 0.2);
		r = r + (((gxa + gxb) * 0.5) * h);
		xa = xa + h;
		tmp = xa < 5000.0;
	}
	return r;
}

float ex5(float Mf, float A) {
	float R = 6400000.0f;
	float G = 6.67428e-11f;
	float Mt = 5.9736e+24f;
	float dt = 0.1f;
	float T = 24.0f * 3600.0f;
	float nombrepas = T / dt;
	float r0 = (400.0f * 10000.0f) + R;
	float vr0 = 0.0f;
	float teta0 = 0.0f;
	float viss = sqrtf(((G * Mt) / r0));
	float vteta0 = viss / r0;
	float rf = R;
	float vrf = 0.0f;
	float tetaf = 0.0f;
	float vl = sqrtf(((G * Mt) / R));
	float vlrad = vl / r0;
	float vtetaf = 1.1f * vlrad;
	float t_i = 0.0f;
	float mf_i = 0.0f;
	float u1_i = 0.0f;
	float u3_i = 0.0f;
	float w1_i = 0.0f;
	float w3_i = 0.0f;
	float u2_i = 0.0f;
	float u4_i = 0.0f;
	float w2_i = 0.0f;
	float w4_i = 0.0f;
	float x = 0.0f;
	float y = 0.0f;
	float i = 1.0f;
	float u1_im1 = r0;
	float u2_im1 = vr0;
	float u3_im1 = teta0;
	float u4_im1 = vteta0;
	float w1_im1 = rf;
	float w2_im1 = vrf;
	float w3_im1 = tetaf;
	float w4_im1 = vtetaf;
	float t_im1 = 0.0f;
	float mf_im1 = Mf;
	int tmp = i < 2000000.0f;
	while (tmp) {
		t_i = t_im1 + dt;
		mf_i = mf_im1 - (A * t_im1);
		u1_i = (u2_im1 * dt) + u1_im1;
		u3_i = (u4_im1 * dt) + u3_im1;
		w1_i = (w2_im1 * dt) + w1_im1;
		w3_i = (w4_im1 * dt) + w3_im1;
		u2_i = ((-G * (Mt / (u1_im1 * u1_im1))) * dt) + ((u1_im1 * u4_im1) * (u4_im1 * dt));
		u4_i = ((-2.0f * (u2_im1 * (u4_im1 / u1_im1))) * dt) + u4_im1;
		float tmp_1;
		if (mf_im1 > 0.0f) {
			tmp_1 = ((A * w2_im1) / (Mf - (A * t_im1))) * dt;
		} else {
			tmp_1 = 0.0f;
		}
		w2_i = (((-G * (Mt / (w1_im1 * w1_im1))) * dt) + ((w1_im1 * w4_im1) * (w4_im1 * dt))) + (tmp_1 + w2_im1);
		float tmp_2;
		if (mf_im1 > 0.0f) {
			tmp_2 = A * ((w4_im1 / (Mf - (A * t_im1))) * dt);
		} else {
			tmp_2 = 0.0f;
		}
		w4_i = ((-2.0f * (w2_im1 * (w4_im1 / w1_im1))) * dt) + (tmp_2 + w4_im1);
		x = u1_i * cosf(u3_i);
		y = u1_i * sinf(u3_i);
		i = i + 1.0f;
		u1_im1 = u1_i;
		u2_im1 = u2_i;
		u3_im1 = u3_i;
		u4_im1 = u4_i;
		w1_im1 = w1_i;
		w2_im1 = w2_i;
		w3_im1 = w3_i;
		w4_im1 = w4_i;
		t_im1 = t_i;
		mf_im1 = mf_i;
		tmp = i < 2000000.0f;
	}
	return x;
}

float ex6(float a11, float a22, float a33, float a44, float b1, float b2, float b3, float b4) {
	float eps = 1e-17f;
	float x_n1 = 0.0f;
	float x_n2 = 0.0f;
	float x_n3 = 0.0f;
	float x_n4 = 0.0f;
	float i = 0.0f;
	float e = 1.0f;
	float x1 = 0.0f;
	float x2 = 0.0f;
	float x3 = 0.0f;
	float x4 = 0.0f;
	int tmp = e > eps;
	while (tmp) {
		x_n1 = (((b1 / a11) - ((0.1f / a11) * x2)) - ((0.2f / a11) * x3)) + ((0.3f / a11) * x4);
		x_n2 = (((b2 / a22) - ((0.3f / a22) * x1)) + ((0.1f / a22) * x3)) - ((0.2f / a22) * x4);
		x_n3 = (((b3 / a33) - ((0.2f / a33) * x1)) + ((0.3f / a33) * x2)) - ((0.1f / a33) * x4);
		x_n4 = (((b4 / a44) + ((0.1f / a44) * x1)) - ((0.2f / a44) * x2)) - ((0.3f / a44) * x3);
		i = i + 1.0f;
		e = fabsf((x_n4 - x4));
		x1 = x_n1;
		x2 = x_n2;
		x3 = x_n3;
		x4 = x_n4;
		tmp = e > eps;
	}
	return x2;
}

float ex7(float x0) {
	float eps = 0.0005f;
	float x_n = 0.0f;
	float e = 1.0f;
	float x = x0;
	float i = 0.0f;
	int tmp = (e > eps) && (i < 100000.0f);
	while (tmp) {
		float f = ((((((x * x) * ((x * x) * x)) - ((10.0f * x) * ((x * x) * x))) + ((40.0f * x) * (x * x))) - ((80.0f * x) * x)) + (80.0f * x)) - 32.0f;
		float ff = (((((5.0f * x) * ((x * x) * x)) - ((40.0f * x) * (x * x))) + ((120.0f * x) * x)) - (160.0f * x)) + 80.0f;
		x_n = x - (f / ff);
		e = fabsf((x - x_n));
		x = x_n;
		i = i + 1.0f;
		tmp = (e > eps) && (i < 100000.0f);
	}
	return x;
}

float ex8(float a11, float a12, float a13, float a14, float a21, float a22, float a23, float a24, float a31, float a32, float a33, float a34, float a41, float a42, float a43, float a44, float v1, float v2, float v3, float v4) {
	float eps = 0.0005f;
	float vx = 0.0f;
	float vy = 0.0f;
	float vz = 0.0f;
	float vw = 0.0f;
	float i = 0.0f;
	float v1_1 = v1;
	float v2_2 = v2;
	float v3_3 = v3;
	float v4_4 = v4;
	float e = 1.0f;
	int tmp = e > eps;
	while (tmp) {
		vx = ((a11 * v1_1) + (a12 * v2_2)) + ((a13 * v3_3) + (a14 * v4_4));
		vy = ((a21 * v1_1) + (a22 * v2_2)) + ((a23 * v3_3) + (a24 * v4_4));
		vz = ((a31 * v1_1) + (a32 * v2_2)) + ((a33 * v3_3) + (a34 * v4_4));
		vw = ((a41 * v1_1) + (a42 * v2_2)) + ((a43 * v3_3) + (a44 * v4_4));
		i = i + 1.0f;
		v1_1 = vx / vw;
		v2_2 = vy / vw;
		v3_3 = vz / vw;
		v4_4 = 1.0f;
		e = fabsf((1.0f - v1_1));
		tmp = e > eps;
	}
	return v1_1;
}

float ex9(float Q11, float Q12, float Q13, float Q21, float Q22, float Q23, float Q31, float Q32, float Q33) {
	float eps = 5e-6f;
	float h1 = 0.0f;
	float h2 = 0.0f;
	float h3 = 0.0f;
	float qj1 = Q31;
	float qj2 = Q32;
	float qj3 = Q33;
	float r1 = 0.0f;
	float r2 = 0.0f;
	float r3 = 0.0f;
	float r = ((qj1 * qj1) + (qj2 * qj2)) + (qj3 * qj3);
	float rjj = 0.0f;
	float e = 10.0f;
	float i = 1.0f;
	float rold = sqrtf(r);
	int tmp = e > eps;
	while (tmp) {
		h1 = ((Q11 * qj1) + (Q21 * qj2)) + (Q31 * qj3);
		h2 = ((Q12 * qj1) + (Q22 * qj2)) + (Q32 * qj3);
		h3 = ((Q13 * qj1) + (Q23 * qj2)) + (Q33 * qj3);
		qj1 = qj1 - (((Q11 * h1) + (Q12 * h2)) + (Q13 * h3));
		qj2 = qj2 - (((Q21 * h1) + (Q22 * h2)) + (Q23 * h3));
		qj3 = qj3 - (((Q31 * h1) + (Q32 * h2)) + (Q33 * h3));
		r1 = r1 + h1;
		r2 = r2 + h2;
		r3 = r3 + h3;
		r = ((qj1 * qj1) + (qj2 * qj2)) + (qj3 * qj3);
		rjj = sqrtf(r);
		e = fabsf((1.0f - (rjj / rold)));
		i = i + 1.0f;
		rold = rjj;
		tmp = e > eps;
	}
	return qj1;
}

