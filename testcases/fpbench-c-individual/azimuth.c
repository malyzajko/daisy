#include <fenv.h>
#include <math.h>
#include <stdint.h>
#define TRUE 1
#define FALSE 0

void __PRECOND(int expr);

double azimuth(double lat1, double lat2, double lon1, double lon2) {
	__PRECOND((0.0 <= lat1) && (lat1 <= 0.4) && (0.5 <= lat2) && (lat2 <= 1.0) && (0.0 <= lon1) && (lon1 <= 3.14159265) && (-3.14159265 <= lon2) && (lon2 <= -0.5));
	double dLon = lon2 - lon1;
	double s_lat1 = sin(lat1);
	double c_lat1 = cos(lat1);
	double s_lat2 = sin(lat2);
	double c_lat2 = cos(lat2);
	double s_dLon = sin(dLon);
	double c_dLon = cos(dLon);
	return atan(((c_lat2 * s_dLon) / ((c_lat1 * s_lat2) - ((s_lat1 * c_lat2) * c_dLon))));
}
