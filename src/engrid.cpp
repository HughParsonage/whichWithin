#include "whichWithin.h"

// [[Rcpp::export]]
IntegerVector engrid(DoubleVector x, DoubleVector y, double r, 
                     double xmin = 1, double xmax = -1,
                     double ymin = 1, double ymax = -1,
                     int nThread = 1) {
  R_xlen_t N = x.length();
  if (N != y.length()) {
    stop("Lengths differ.");
  }
  
  double rxy[4] = {xmin, xmax, ymin, ymax};
  aminmax_dbl(rxy, x, y, nThread);
  

  
  double rx = rxy[1] - rxy[0];
  double ry = rxy[3] - rxy[2];
  
  double rn = r / ((rx < ry) ? ry : rx);
  int b = std::ceil(1 / rn);
  rn = std::nextafter(rn, INFINITY);
  
  
  
  IntegerVector out = no_init(N);
  
  for (R_xlen_t i = 0; i < N; ++i) {
    double xi = x[i], yi = y[i];
    double xn = (xi - rxy[0]) / rx;
    int xg = xn / rn;
    double yn = (yi - rxy[2]) / ry;
    int yg = yn / rn;
    out[i] = xg + b * yg; 
  }
  return out;
}

