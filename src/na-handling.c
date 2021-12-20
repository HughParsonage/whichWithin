#include "whichWithin.h"

double first_non_na_dbl(SEXP xx, SEXP ffill) {
  if (!isReal(xx) || !isReal(ffill)) {
    error("(Cfirst_non_na_dbl): must be REALSXP"); // # nocov
  }
  R_xlen_t N = xlength(xx);
  const double fill = asReal(ffill);
  const double * x = REAL(xx);
  for (R_xlen_t i = 0; i < N; ++i) {
    if (!ISNAN(x[i])) {
      return x[i];
    }
  }
  return fill;
}

SEXP Cfirst_non_na_dbl(SEXP xx, SEXP ffill) {
  return ScalarReal(first_non_na_dbl(xx, ffill));
}

