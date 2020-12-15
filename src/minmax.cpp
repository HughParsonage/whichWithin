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

void aminmax_dbl(double ans[4], DoubleVector x, DoubleVector y, int nThread) {
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
  
#pragma omp parallel for num_threads(nThread) reduction(min : xmin,ymin) reduction(max : xmax,ymax)
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

// [[Rcpp::export(rng = false)]]
DoubleVector do_minmax(DoubleVector x, DoubleVector y, int nThread = 1) {
  double ans[4] = {1, -1, 1, -1};
  aminmax_dbl(ans, x, y, nThread);
  DoubleVector out = no_init(4);
  for (int i = 0; i < 4; ++i) {
    out[i] = ans[i];
  }
  return out;
}

