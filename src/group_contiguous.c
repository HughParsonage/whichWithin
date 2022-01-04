#include "whichWithin.h"

SEXP Cgroup_contiguous(SEXP x, SEXP rr) {
  if (!isInteger(x)) {
    error("Internal error: x was type '%s' but must be integer", type2char(TYPEOF(x))); // # nocov
  }
  if (!isInteger(rr)) {
    error("Internal error: r was type '%s' but must be integer", type2char(TYPEOF(rr))); // # nocov
  }
  const int r = asInteger(rr);
  const int * xp = INTEGER(x);
  R_xlen_t N = xlength(x);
  if (N <= 1) {
    return IntegerN(N); // isolated always
  }
  // first point special
  SEXP ans = PROTECT(allocVector(INTSXP, N));
  int * restrict ansp = INTEGER(ans);
  memset(ansp, 0, N * sizeof(int));
  int64_t x64p0 = xp[0], x64pN = xp[N - 1];
  if ((x64pN - x64p0) >= INT_MAX) {
    error("xpN - xp0 exceeds INT_MAX"); // # nocov
  }
  int xp0 = xp[0], xp1 = xp[1];
  unsigned int curr_grp = 0;
  if (xp1 - xp0 <= r) {
    ansp[0] = 1;
    curr_grp = 1;
  }
  
  for (R_xlen_t i = 1; i < N; ++i) {
    int xp_i0 = xp[i - 1];
    int xp_i1 = xp[i];
    int r_i = xp_i1 - xp_i0;
    if (r_i > r) {
      ++curr_grp;
    }
    ansp[i] = curr_grp;
  }
  UNPROTECT(1);
  return ans;
}
