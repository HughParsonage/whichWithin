#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export(rng = false)]]
LogicalVector is_sorted2(DoubleVector x, DoubleVector y, LogicalVector s2) {
  R_xlen_t N = x.length();
  if (s2.length() == 1) {
    if (s2[0] != NA_LOGICAL) {
      return s2;
    }
    LogicalVector out(1);
    for (R_xlen_t i = 1; i < N; ++i) {
      if (x[i - 1] > x[i]) {
        return out;
      }
    }
    out[0] = TRUE;
    return out;
  }
  LogicalVector out(2);
  if (s2.length() != 2) {
    return out;
  }
  if (s2[0] != NA_LOGICAL && s2[1] != NA_LOGICAL) {
    return s2;
  }
  bool y_not_sorted = false;
  
  for (R_xlen_t i = 1; i < N; ++i) {
    double di = x[i] - x[i - 1];
    if (di > 0) {
      continue;
    }
    if (di < 0) {
      return out;
    }
    if (y_not_sorted) {
      continue;
    }
    if (y[i - 1] > y[i]) {
      y_not_sorted = true;
    }
    
  }
  out[0] = TRUE;
  out[1] = !y_not_sorted;
  return out;
}
