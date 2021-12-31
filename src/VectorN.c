#include "whichWithin.h"

SEXP IntegerN(R_xlen_t N) {
  SEXP ans = PROTECT(allocVector(INTSXP, N));
  int * restrict ansp = INTEGER(ans);
  memset(ansp, 0, sizeof(int) * N);
  return ans;
}