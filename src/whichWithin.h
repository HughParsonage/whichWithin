#ifndef whichWithin_H
#define whichWithin_H

#ifdef _OPENMP
#include <omp.h>
#endif

#include <R.h>
#define USE_RINTERNALS
#include <Rinternals.h>
#include <stdint.h> // for uint64_t rather than unsigned long long
#include <stdbool.h>
#include <inttypes.h> // for printing uint64_t
#include <math.h>
#include <ctype.h>


#define GG_RES 2076;
int imin(int a, int b);
int imax(int a, int b);
void aminmax_dbl(double ans[4], const double * x, const double * y, R_xlen_t N);
void aminmax1(double ans[2], const double * x, R_xlen_t N);
void vminmaxi(int ans[2], const int * xp, R_xlen_t N);

void sinusoidal(R_xlen_t N, double * x, double * y, const double * lat, const double * lon, double lambda0);
double euclid_dist_sq(double x1, double y1, double x2, double y2);
double euclid_dist(double x1, double y1, double x2, double y2);
double haversine_dist(double olat1, double olon1, double olat2, double olon2);
double haversine_dist_klatlon(double lat1, double lon1, double olat2, double olon2, double coslat1);
double haversine_dist_kdueeast(double lat1, double lon1, double olat2, double olon2, double coslat1);
double delta_lon_given_R(double coslat1, double sqrtR);
bool within_one_1km_aus(double olat1, double olon1, double olat2, double olon2);

bool is_sorted(SEXP x);
void verify_sorted2(R_xlen_t N, SEXP x, SEXP y, int err_no);
void verify_sorted_int(R_xlen_t N, const int * xp, const char * nx);

#define INGRID_SORTED2_ERR_NO 11
#define DO_WHICH_WITHIN_SORTED2_ERR_NO 12

// const double EARTH_RADIUS_KM = 6378.137;
#define EARTH_RADIUS_KM 6371.0

// isntEqui
void checkEquiRealReal(SEXP x, SEXP y, const char * nx, const char * ny);
void checkEquiIntInt(SEXP x, SEXP y, const char * nx, const char * ny);

// VectorN
SEXP IntegerN(R_xlen_t N);


#endif
