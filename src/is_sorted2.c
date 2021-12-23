#include "whichWithin.h"

SEXP Cis_sorted2(SEXP xx, SEXP yy, SEXP ss2) {
  if (!isReal(xx) || !isReal(yy) || !isLogical(ss2)) {
    error("(Cis_sorted2): wrong types."); // # nocov
  }
  if (xlength(xx) != xlength(yy)) {
    error("(Cis_sorted2): lengths differ."); // # nocov
  }
  R_xlen_t N = xlength(xx);
  const double * x = REAL(xx);
  const double * y = REAL(yy);
  if (xlength(ss2) == 1) {
    if (LOGICAL(ss2)[0] != NA_LOGICAL) {
      return ss2;
    }
    
    for (R_xlen_t i = 1; i < N; ++i) {
      if (x[i - 1] > x[i]) {
        return ScalarLogical(0);
      }
    }
    return ScalarLogical(1);
  }
  if (LOGICAL(ss2)[0] != NA_LOGICAL && LOGICAL(ss2)[1] != NA_LOGICAL) {
    return ss2;
  }
  SEXP out = PROTECT(allocVector(LGLSXP, 2));
  LOGICAL(out)[0] = 0;
  LOGICAL(out)[1] = 0;
  if (xlength(ss2) != 2) {
    UNPROTECT(1);
    return out;
  }
  bool y_not_sorted = false;
  for (R_xlen_t i = 1; i < N; ++i) {
    double di = x[i] - x[i - 1];
    if (di > 0) {
      continue;
    }
    if (di < 0) {
      UNPROTECT(1);
      return out;
    }
    if (y_not_sorted) {
      continue;
    }
    if (y[i - 1] > y[i]) {
      y_not_sorted = true;
    }
    
  }
  LOGICAL(out)[0] = 1;
  LOGICAL(out)[1] = !y_not_sorted;
  UNPROTECT(1);
  return out;
}
