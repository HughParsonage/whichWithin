#include "whichWithin.h"

static void avg_lambda(double & lambda0, DoubleVector lon, R_xlen_t N) {
  if (ISNAN(lambda0)) {
    lambda0 = lon[N / 2];
    if (ISNAN(lambda0)) {
      double s = lon[0];
      for (R_xlen_t i = 1; i < N; ++i) {
        s += lon[i];
      }
      lambda0 = s / ((double)N);
    }
  }
}

void sinusoidal(R_xlen_t N,
                DoubleVector x, 
                DoubleVector y,
                DoubleVector lat,
                DoubleVector lon,
                double lambda_0) {
  double radf = M_PI / 180.0;
  
  // choose the middle lambda (hopefully the median)
  // or one provided by the user
  avg_lambda(lambda_0, lon, N);
  
  for (R_xlen_t i = 0; i < N; ++i) {
    if (ISNAN(lat[i]) || ISNAN(lon[i])) {
      x[i] = R_PosInf;
      y[i] = R_NegInf;
      continue;
    }
    y[i] = lat[i] * radf;
    x[i] = (lon[i] - lambda_0) * radf;
    double cos_phi = std::cos(y[i]);
    x[i] *= cos_phi;
  }
}



// [[Rcpp::export(rng = false)]]
List Sinusoidal(DoubleVector lat, DoubleVector lon, double lambda0 = NA_REAL) {
  R_xlen_t N = lat.length();
  if (lon.length() != N) {
    stop("lon.length() != N");
  }
  DoubleVector x = no_init(N);
  DoubleVector y = no_init(N);
  
  
  double radf = M_PI / 180.0;
  double lambda_0 = ISNAN(lambda0) ? lon[N / 2] : lambda0; 
  
  for (R_xlen_t i = 0; i < N; ++i) {
    if (ISNAN(lat[i]) || ISNAN(lon[i])) {
      Rcerr << "i = " << (i + 1) << "\n";
      stop("lat or lon was NaN at above position.");
    }
    y[i] = lat[i] * radf;
    x[i] = (lon[i] - lambda_0) * radf;
    double cos_phi = std::cos(y[i]);
    x[i] *= cos_phi;
  }
  return List::create(Named("x") = x, Named("y") = y);
}

// [[Rcpp::export(rng = false)]]
DoubleVector dist_sinusoidal(DoubleVector lat1, DoubleVector lon1, 
                             DoubleVector lat2, DoubleVector lon2,
                             double lambda0 = NA_REAL) {
  R_xlen_t N = lat1.length();
  if (lon1.length() != N || lat2.length() != N || lon2.length() != N) {
    stop("lon.length() != N");
  }
  DoubleVector x1 = no_init(N);
  DoubleVector x2 = no_init(N);
  DoubleVector y1 = no_init(N);
  DoubleVector y2 = no_init(N);
  
  
  double radf = M_PI / 180.0;
  double lambda_0 = ISNAN(lambda0) ? lon1[N / 2] : lambda0; 
  
  for (R_xlen_t i = 0; i < N; ++i) {
    y1[i] = lat1[i] * radf;
    y2[i] = lat2[i] * radf;
    x1[i] = (lon1[i] - lambda_0) * radf;
    x2[i] = (lon2[i] - lambda_0) * radf;
    double cos_phi1 = std::cos(y1[i]);
    double cos_phi2 = std::cos(y2[i]);
    x1[i] *= cos_phi1;
    x2[i] *= cos_phi2;
  }
  
  DoubleVector d = no_init(N);
  
  for (R_xlen_t i = 0; i < N; ++i) {
    d[i] = euclid_dist(x1[i], y1[i], x2[i], y2[i]) * EARTH_RADIUS_KM;
  }
  
  return d;
}




