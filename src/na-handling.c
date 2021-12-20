#include "whichWithin.h"

SEXP Cfirst_non_na_dbl(SEXP xx, SEXP ffill) {
  if (!isReal(xx) || !isReal(ffill)) {
    error("(Cfirst_non_na_dbl): must be REALSXP"); // # nocov
  }
  R_xlen_t N = xlength(xx);
  for (R_xlen_t i = 0; i < N; ++i) {
    if (!ISNAN(x[i])) {
      return x[i];
    }
  }
  return fill;
}

