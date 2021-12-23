#include "whichWithin.h"

static void avg_lambda(double * lambda0, const double * lon, R_xlen_t N) {
  if (ISNAN(*lambda0)) {
    *lambda0 = lon[N / 2];
    if (ISNAN(*lambda0)) {
      double s = lon[0];
      for (R_xlen_t i = 1; i < N; ++i) {
        s += lon[i];
      }
      *lambda0 = s / ((double)N);
    }
  }
}

void sinusoidal(R_xlen_t N,
                double * x, 
                double * y,
                const double * lat,
                const double * lon,
                double lambda_0) {
  double radf = M_PI / 180.0;
  
  // choose the middle lambda (hopefully the median)
  // or one provided by the user
  avg_lambda(&lambda_0, lon, N);
  
  for (R_xlen_t i = 0; i < N; ++i) {
    if (ISNAN(lat[i]) || ISNAN(lon[i])) {
      x[i] = R_PosInf;
      y[i] = R_NegInf;
      continue;
    }
    y[i] = lat[i] * radf;
    x[i] = (lon[i] - lambda_0) * radf;
    double cos_phi = cos(y[i]);
    x[i] *= cos_phi;
  }
}




SEXP CSinusoidal(SEXP llat, SEXP llon, SEXP llambda0) {
  if (!isReal(llat) || !isReal(llon) || !isReal(llambda0)) {
    error("Internal error (Sinusoidal): wrong types."); // # nocov
  }
  double lambda0 = asReal(llambda0);
  R_xlen_t N = xlength(llat);
  if (xlength(llon) != N) {
    error("lon.length() != N");
  }
  const double * lat = REAL(llat);
  const double * lon = REAL(llon);
  SEXP xx = PROTECT(allocVector(REALSXP, N));
  SEXP yy = PROTECT(allocVector(REALSXP, N));
  double * x = REAL(xx);
  double * y = REAL(yy);
  
  double radf = M_PI / 180.0;
  double lambda_0 = ISNAN(lambda0) ? lon[N / 2] : lambda0; 
  
  for (R_xlen_t i = 0; i < N; ++i) {
    if (ISNAN(lat[i]) || ISNAN(lon[i])) {
      error("lat or lon was NaN at position %lld.", i + 1);
    }
    y[i] = lat[i] * radf;
    x[i] = (lon[i] - lambda_0) * radf;
    double cos_phi = cos(y[i]);
    x[i] *= cos_phi;
  }
  SEXP ans = PROTECT(allocVector(VECSXP, 2));
  SET_VECTOR_ELT(ans, 0, xx);
  SET_VECTOR_ELT(ans, 1, yy);
  UNPROTECT(3);
  return ans;
}


SEXP Cdist_sinusoidal(SEXP llat1, 
                      SEXP llon1, 
                     SEXP llat2, 
                     SEXP llon2,
                     SEXP llambda0) {
  if (!isReal(llat1) || !isReal(llon1) || !isReal(llat2) || !isReal(llon2)) {
    error("Internal error(Cdist_sinusoidal): long types."); // # nocov
  }
  
  const double * lat1 = REAL(llat1);
  const double * lat2 = REAL(llat2);
  const double * lon1 = REAL(llon1);
  const double * lon2 = REAL(llon2);
  
  double lambda0 = asReal(llambda0);
  R_xlen_t N = xlength(llat1);
  if (xlength(llon1) != N || xlength(llat2) != N || xlength(llon2) != N) {
    error("lon.length() != N"); // # nocov
  }
  SEXP xx1 = PROTECT(allocVector(REALSXP, N));
  SEXP xx2 = PROTECT(allocVector(REALSXP, N));
  SEXP yy1 = PROTECT(allocVector(REALSXP, N));
  SEXP yy2 = PROTECT(allocVector(REALSXP, N));
  
  double * x1 = REAL(xx1);
  double * x2 = REAL(xx2);
  double * y1 = REAL(yy1);
  double * y2 = REAL(yy2);
  
  double radf = M_PI / 180.0;
  double lambda_0 = ISNAN(lambda0) ? lon1[N / 2] : lambda0; 
  
  for (R_xlen_t i = 0; i < N; ++i) {
    y1[i] = lat1[i] * radf;
    y2[i] = lat2[i] * radf;
    x1[i] = (lon1[i] - lambda_0) * radf;
    x2[i] = (lon2[i] - lambda_0) * radf;
    double cos_phi1 = cos(y1[i]);
    double cos_phi2 = cos(y2[i]);
    x1[i] *= cos_phi1;
    x2[i] *= cos_phi2;
  }
  
  SEXP dd = PROTECT(allocVector(REALSXP, N));
  double * d = REAL(dd);
  
  for (R_xlen_t i = 0; i < N; ++i) {
    d[i] = euclid_dist(x1[i], y1[i], x2[i], y2[i]) * EARTH_RADIUS_KM;
  }
  UNPROTECT(1);
  return dd;
}




