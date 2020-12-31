#ifndef whichWithin_H
#define whichWithin_H

#ifdef _OPENMP
#include <omp.h>
#endif

#include <Rcpp.h>
using namespace Rcpp;



void aminmax_dbl(double ans[4], DoubleVector x, DoubleVector y);
void aminmax1(double ans[2], DoubleVector x, R_xlen_t N);
void sinusoidal(R_xlen_t N, DoubleVector x, DoubleVector y, DoubleVector lat, DoubleVector lon, double lambda0);
double euclid_dist_sq(double x1, double y1, double x2, double y2);
double euclid_dist(double x1, double y1, double x2, double y2);
double haversine_dist(double olat1, double olon1, double olat2, double olon2);
double haversine_dist_klatlon(double lat1, double lon1, double olat2, double olon2, double coslat1);
double haversine_dist_kdueeast(double lat1, double lon1, double olat2, double olon2, double coslat1);
double delta_lon_given_R(double coslat1, double sqrtR);
bool within_one_1km_aus(double olat1, double olon1, double olat2, double olon2);

void verify_sorted2(R_xlen_t N, DoubleVector x, DoubleVector y, int err_no);

const int INGRID_SORTED2_ERR_NO = 11;
const int DO_WHICH_WITHIN_SORTED2_ERR_NO = 12;

// const double EARTH_RADIUS_KM = 6378.137;
const double EARTH_RADIUS_KM = 6371.0;



#endif
