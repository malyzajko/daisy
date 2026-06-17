#include <fenv.h>
#include <math.h>
#include <stdint.h>
#define TRUE 1
#define FALSE 0

void __PRECOND(int expr);

double sphere(double x, double r, double lat, double lon) {
    __PRECOND((-10.0 <= x) && (x <= 10.0) && (0.0 <= r) && (r <= 10.0) && (-1.570796 <= lat) && (lat <= 1.570796) && (-3.14159265 <= lon) && (lon <= 3.14159265));
	double sinLat = sin(lat);
	double cosLon = cos(lon);
	return x + ((r * sinLat) * cosLon);
}
