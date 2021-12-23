#include "whichWithin.h"


SEXP Cengrid(SEXP xx, SEXP yy, SEXP rr) {
  if (!isReal(xx) || !isReal(yy)) {
    error("xx and yy were not REALSXP."); // # nocov
  }
  const double * x = REAL(xx);
  const double * y = REAL(yy);
  double r = asReal(rr);
  R_xlen_t N = xlength(xx);
  if (N != xlength(yy)) {
    error("Lengths differ."); // # nocov
  }
  
  double rxy[4] = {1, -1, 1, -1};
  aminmax_dbl(rxy, x, y, N);
  
  double rx = rxy[1] - rxy[0];
  double ry = rxy[3] - rxy[2];
  
  double rn = r / ((rx < ry) ? ry : rx);
  int b = ceil(1 / rn);
  rn = nextafter(rn, INFINITY);
  
  SEXP ans = PROTECT(allocVector(INTSXP, N));
  int * restrict ansp = INTEGER(ans);
  
  for (R_xlen_t i = 0; i < N; ++i) {
    double xi = x[i], yi = y[i];
    double xn = (xi - rxy[0]) / rx;
    int xg = xn / rn;
    double yn = (yi - rxy[2]) / ry;
    int yg = yn / rn;
    ansp[i] = xg + b * yg; 
  }
  UNPROTECT(1);
  return ans;
}


SEXP high_prec_int(SEXP xx) {
  R_xlen_t N = xlength(xx);
  const double * x = REAL(xx);
  SEXP ans = PROTECT(allocVector(INTSXP, N));
  int * restrict ansp = INTEGER(ans);
  for (R_xlen_t i = 0; i < N; ++i) {
    ansp[i] = (10000 * x[i]);
  }
  UNPROTECT(1);
  return ans;
}




