#include "whichWithin.h"

double sinhalfsq (double x) {
  const double o = sin(x / 2);
  return o * o;
}

double haversine_dist(double olat1, double olon1, double olat2, double olon2) {
  const double lat1 = olat1 * (M_PI / 180) ;
  const double lat2 = olat2 * (M_PI / 180) ;
  const double lon1 = olon1 * (M_PI / 180) ;
  const double lon2 = olon2 * (M_PI / 180) ;
  
  
  // const double delta_lat = (lat1 > lat2) ? (lat1 - lat2) : (lat2 - lat1) ;
  const double delta_lat = fabs(lat1 - lat2);
  // const double delta_lon = (lon1 > lon2) ? (lon1 - lon2) : (lon2 - lon1) ;
  const double delta_lon = fabs(lon1 - lon2);
  
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


double haversine_dist_uys(double olat1, double olon1, double olat2, double olon2) {
  // unitless, sorted in the latitutde
  const double lat1 = olat1 * (M_PI / 180) ;
  const double lat2 = olat2 * (M_PI / 180) ;
  const double lon1 = olon1 * (M_PI / 180) ;
  const double lon2 = olon2 * (M_PI / 180) ;
  
  const double delta_lat = lat2 - lat1;
  const double delta_lon = fabs(lon2 - lon1);
  
  double out = 0;
  double den = cos(lat1) * cos(lat2) * sinhalfsq(delta_lon);
  out = sinhalfsq(delta_lat);
  out += den;
  return out;
}

double haversine_dist_klatlon(double lat1, double lon1, double lat2, double lon2, double coslat1) {
  const double delta_lat = lat2 - lat1;
  const double delta_lon = fabs(lon2 - lon1);
  double out = sinhalfsq(delta_lat) + coslat1 * cos(lat2) * sinhalfsq(delta_lon);
  return out;
}


double euclid_dist_sq(double x1, double y1, double x2, double y2) {
  double dx = x2 - x1;
  double dy = y2 - y1;
  return (dx * dx) + (dy * dy);
}

double euclid_dist(double x1, double y1, double x2, double y2) {
  return sqrt(euclid_dist_sq(x1, y1, x2, y2));
}

double aus_bbox[4] = {-43.5, -9, 96.5, 168};

// (double olat1, double olon1, double olat2, double olon2) 
SEXP Chaversine_dist(SEXP Olat1, SEXP Olon1, SEXP Olat2, SEXP Olon2, SEXP M) {
  double olat1 = asReal(Olat1);
  double olon1 = asReal(Olon1);
  double olat2 = asReal(Olat2);
  double olon2 = asReal(Olon2);
  return ScalarReal(haversine_dist(olat1, olon1, olat2, olon2));
}

