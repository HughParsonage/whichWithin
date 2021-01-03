#include "whichWithin.h"

void vmin(double & xmin, double xi) {
  if (xi < xmin) {
    xmin = xi;
  }
}
void vmax(double & xmax, double xi) {
  if (xi > xmax) {
    xmax = xi;
  }
}

void vmin(int & xmax, int xi) {
  if (xi < xmax) {
    xmax = xi;
  }
}

void vmax(int & xmax, int xi) {
  if (xi > xmax) {
    xmax = xi;
  }
}

void vmaxmin(IntegerVector x, int & xmax, int & xmin) {
  R_xlen_t N = x.length();
  for (R_xlen_t i = 0; i < N; ++i) {
    int xi = x[i];
    vmax(xmax, xi);
    vmin(xmin, xi);
  }
}





void aminmax_dbl(double ans[4], DoubleVector x, DoubleVector y) {
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
  R_xlen_t N = x.length();
  
  for (R_xlen_t i = 0; i < N; ++i) {
    double xi = x[i], yi = y[i];
    vmin(xmin, xi);
    vmin(ymin, yi);
    vmax(xmax, xi);
    vmax(ymax, yi);
  }
  ans[0] = xmin;
  ans[1] = xmax;
  ans[2] = ymin;
  ans[3] = ymax;
}

void aminmax1(double ans[2], DoubleVector x, R_xlen_t N) {
  double xmin = x[0];
  double xmax = x[0];
  for (R_xlen_t i = 1; i < N; ++i) {
    double xi = x[i];
    vmin(xmin, xi);
    vmax(xmax, xi);
  }
  ans[0] = xmin;
  ans[1] = xmax;
}

// [[Rcpp::export(rng = false)]]
DoubleVector do_minmax(DoubleVector x, DoubleVector y) {
  double ans[4] = {1, -1, 1, -1};
  aminmax_dbl(ans, x, y);
  DoubleVector out = no_init(4);
  for (int i = 0; i < 4; ++i) {
    out[i] = ans[i];
  }
  return out;
}





