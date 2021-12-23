#include "whichWithin.h"

void dvmin(double * xmin, double xi) {
  if (xi < *xmin) {
    *xmin = xi;
  }
}
void dvmax(double * xmax, double xi) {
  if (xi > *xmax) {
    *xmax = xi;
  }
}

void ivmin(int * xmax, int xi) {
  if (xi < *xmax) {
    *xmax = xi;
  }
}

void ivmax(int * xmax, int xi) {
  if (xi > *xmax) {
    *xmax = xi;
  }
}





void aminmax_dbl(double ans[4], const double * x, const double * y, R_xlen_t N) {
  double xmin = ans[0];
  double xmax = ans[1];
  double ymin = ans[2];
  double ymax = ans[3];
  if (xmin <= xmax && ymin <= ymax) {
    // assume already correct
    return;
  }
  xmin = x[0];
  xmax = x[0];
  ymin = y[0];
  ymax = y[0];
  
  for (R_xlen_t i = 0; i < N; ++i) {
    double xi = x[i], yi = y[i];
    dvmin(&xmin, xi);
    dvmin(&ymin, yi);
    dvmax(&xmax, xi);
    dvmax(&ymax, yi);
  }
  ans[0] = xmin;
  ans[1] = xmax;
  ans[2] = ymin;
  ans[3] = ymax;
}

void aminmax1(double ans[2], const double * x, R_xlen_t N) {
  double xmin = x[0];
  double xmax = x[0];
  for (R_xlen_t i = 1; i < N; ++i) {
    double xi = x[i];
    dvmin(&xmin, xi);
    dvmax(&xmax, xi);
  }
  ans[0] = xmin;
  ans[1] = xmax;
}

SEXP Cdo_minmax(SEXP xx, SEXP yy) {
  double ans[4] = {1, -1, 1, -1};
  const double * x = REAL(xx);
  const double * y = REAL(yy);
  aminmax_dbl(ans, x, y, xlength(xx));
  SEXP out = allocVector(REALSXP, 4);
  for (int i = 0; i < 4; ++i) {
    REAL(out)[i] = ans[i];
  }
  return out;
}





