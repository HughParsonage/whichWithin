#include "whichWithin.h"

double sinhalfsq (double x) {
  const double o = sin(x / 2);
  return o * o;
}

// [[Rcpp::export(rng = false)]]
double haversine_dist(double olat1, double olon1, double olat2, double olon2) {
  const double lat1 = olat1 * (M_PI / 180) ;
  const double lat2 = olat2 * (M_PI / 180) ;
  const double lon1 = olon1 * (M_PI / 180) ;
  const double lon2 = olon2 * (M_PI / 180) ;
  
  
  // const double delta_lat = (lat1 > lat2) ? (lat1 - lat2) : (lat2 - lat1) ;
  const double delta_lat = std::fabs(lat1 - lat2);
  // const double delta_lon = (lon1 > lon2) ? (lon1 - lon2) : (lon2 - lon1) ;
  const double delta_lon = std::fabs(lon1 - lon2);
  
  // 6371 * 2 * asin(sqrt(sin(d_lat / 2)^2 + cos(lat1) * cos(lat2) * sin(d_lon / 2)^2))
  double out = 0;
  double den = cos(lat1) * cos(lat2) * sinhalfsq(delta_lon);
  out = sinhalfsq(delta_lat);
  out += den;
  out = sqrt(out);
  out = asin(out);
  out *= 6371;
  out *= 2;
  return out;
}

// [[Rcpp::export(rng = false)]]
double haversine_dist_uys(double olat1, double olon1, double olat2, double olon2) {
  // unitless, sorted in the latitutde
  const double lat1 = olat1 * (M_PI / 180) ;
  const double lat2 = olat2 * (M_PI / 180) ;
  const double lon1 = olon1 * (M_PI / 180) ;
  const double lon2 = olon2 * (M_PI / 180) ;
  
  const double delta_lat = lat2 - lat1;
  const double delta_lon = std::fabs(lon2 - lon1);
  
  double out = 0;
  double den = cos(lat1) * cos(lat2) * sinhalfsq(delta_lon);
  out = sinhalfsq(delta_lat);
  out += den;
  return out;
}

// [[Rcpp::export(rng = false)]]
double haversine_dist_klatlon(double lat1, double lon1, double lat2, double lon2, double coslat1) {
  const double delta_lat = lat2 - lat1;
  const double delta_lon = std::fabs(lon2 - lon1);
  double out = sinhalfsq(delta_lat) + coslat1 * cos(lat2) * sinhalfsq(delta_lon);
  return out;
}

double haversine_dist_kdueeast(double lat1, double lon1, double lat2, double olon2, double coslat1) {
  lat2 = lat1;
  const double lon2 = olon2 * (M_PI / 180) ;
  
  // const double delta_lat = 0;
  const double delta_lon = std::fabs(lon2 - lon1);
  double out = coslat1 * coslat1 * sinhalfsq(delta_lon);
  return out;
}

// [[Rcpp::export(rng = false)]]
double delta_lon_given_R(double coslat1, double sqrtR) {
  return 2 * asin(sqrtR / coslat1);
}

// [[Rcpp::export(rng = false)]]
double delta_olon_given_R(double coslat1, double sqrtR) {
  return 360 * asin(sqrtR / coslat1) / M_PI;
}


double euclid_dist_sq(double x1, double y1, double x2, double y2) {
  double dx = x2 - x1;
  double dy = y2 - y1;
  return (dx * dx) + (dy * dy);
}

double euclid_dist(double x1, double y1, double x2, double y2) {
  return sqrt(euclid_dist_sq(x1, y1, x2, y2));
}

bool diff_leq_1(int x, int y) {
  unsigned int ux = x;
  unsigned int uy = y;
  unsigned int d_plus_1 = x - y;
  d_plus_1++;
  return d_plus_1 <= 2;
}

double aus_bbox[4] = {-43.5, -9, 96.5, 168};

bool within_one_16km(double olat1, double olon1, double olat2, double olon2) {
  int olon101 = (olon1 + 180) * 10;
  int olon102 = (olon2 + 180) * 10;
  int olat101 = (olat1 + 180) * 10;
  int olat102 = (olat2 + 180) * 10;
  
  return (olat101 == olat102) && (olon101 == olon102);
}


// [[Rcpp::export(rng = false)]]
DoubleVector haversine_distance_par(DoubleVector lat1, DoubleVector lon1, 
                                    DoubleVector lat2, DoubleVector lon2, 
                                    int nThread = 1) {
  R_xlen_t N = lat1.length();
  if (lon1.length() != N ||
      lat2.length() != N ||
      lon2.length() != N) {
    stop("Bad length.");
  }
  DoubleVector out = no_init(N);
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
  for (R_xlen_t i = 0; i < N; ++i) {
    out[i] = haversine_dist(lat1[i], lon1[i], lat2[i], lon2[i]);
  }
  return out;
}
