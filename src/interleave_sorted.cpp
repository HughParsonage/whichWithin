#include "whichWithin.h"

// [[Rcpp::export(rng = false)]]
IntegerVector interleave_sorted(IntegerVector x, IntegerVector y) {
  R_xlen_t xN = x.length();
  R_xlen_t yN = y.length();
  IntegerVector out = no_init(xN + yN);
  R_xlen_t i = 0;
  R_xlen_t j = 0;
  R_xlen_t k = 0;
  while (i < xN && j < yN) {
    if (x[i] <= y[j]) {
      out[k] = x[i];
      ++i;
    } else {
      out[k] = y[j];
      ++j;
    }
    ++k;
  }
  while (i < xN) {
    out[k++] = x[i++];
  }
  while (j < yN) {
    out[k++] = y[j++];
  }
  return out;
}

// [[Rcpp::export(rng = false)]]
IntegerVector unique_sorted(IntegerVector x) {
  R_xlen_t N = x.length();
  std::vector<int> o;
  o.reserve(N);
  o.push_back(x[0]);
  for (R_xlen_t i = 1; i < N; ++i) {
    if (x[i] != x[i - 1]) {
      o.push_back(x[i]);
    }
  }
  return wrap(o);
}

// [[Rcpp::export(rng = false)]]
IntegerVector unique_sorted_inplace(IntegerVector x) {
  R_xlen_t N = x.length();
  R_xlen_t p = 0;
  for (R_xlen_t i = 1; i < N; ++i) {
    bool newi = x[i] != x[i - 1];
    p += newi;
    x[p] = x[i];
  }
  ++p; // since p does not include the first element.
  IntegerVector out = no_init(p);
  for (R_xlen_t i = 0; i < p; ++i) {
    out[i] = x[i];
  }
  return out;
}

/*
 
 * This function is like setorderv(DT, "ord")[["ans"]]
LogicalVector restore_orig_order(LogicalVector ans, IntegerVector ord, int nThread = 1) {
  int N = ans.length();
  if (N != ord.length()) {
    stop("N != ord.length()"); // # nocov
  }
  LogicalVector out = no_init(N);
  
#pragma omp parallel for num_threads(nThread)
  for (int i = 0; i < N; ++i) {
    int oi = ord[i] - 1;
    out[oi] = ans[i];
  }
  return out;
}
 */

