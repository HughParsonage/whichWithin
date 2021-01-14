#include "whichWithin.h"

// [[Rcpp::export(rng = false)]]
double first_non_na_dbl(DoubleVector x, double fill = 0) {
  R_xlen_t N = x.length();
  for (R_xlen_t i = 0; i < N; ++i) {
    if (!ISNAN(x[i])) {
      return x[i];
    }
  }
  return fill;
}

