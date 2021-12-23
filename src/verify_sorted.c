#include "whichWithin.h"

void verify_sorted2(R_xlen_t N, SEXP xx, SEXP yy, int err_no) {
  const double * x = REAL(xx);
  const double * y = REAL(yy);
  bool isnt_sorted2 = false;
  for (R_xlen_t i = 1; i < N; ++i) {
    double x0 = x[i - 1];
    double x1 = x[i];
    double y0 = y[i - 1];
    double y1 = y[i];
    isnt_sorted2 = 
      isnt_sorted2 ||
      (x0 > x1) ||
      (x0 == x1 && y0 > y1);
  }
  
  if (isnt_sorted2) {
    error("Error #%d, Internal error: wasn't sorted in x, y.", err_no);
  }
}
