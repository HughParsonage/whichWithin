#include "whichWithin.h"

R_xlen_t sum_first_k(R_xlen_t k) {
  // complex only due to (k / 2) being floor(k / 2)
  return (k % 2) ? (k * ((k + 1) / 2)) : ((k / 2) * (k + 1));
}

// function(k) sum((n - 1:k))
R_xlen_t sum_n_minus_seq(R_xlen_t j, R_xlen_t N) {
  R_xlen_t N0 = N - 1;
  return -j * (j - 1 - 2*N0) / 2;
}

R_xlen_t sum_n_le(R_xlen_t N) {
  R_xlen_t o = 0;
  for (R_xlen_t i = 0; i < N; ++i) {
    for (R_xlen_t j = 0; j < N; ++j) {
      o += (i < j);
    }
  }
  return o;
}

R_xlen_t sum_n_le2(R_xlen_t N) {
  return (N * (N - 1)) / 2;
}

// [[Rcpp::export(rng = false)]]
List Z4(DoubleVector x, DoubleVector y) {
  R_xlen_t n = x.length();
  R_xlen_t N = sum_n_le2(n);
  IntegerVector id1(N);
  IntegerVector id2(N);
  DoubleVector lat1(N);
  DoubleVector lat2(N);
  DoubleVector lon1(N);
  DoubleVector lon2(N);
  
  R_xlen_t k = 0;
  for (R_xlen_t i = 0; i < n; ++i) {
    for (R_xlen_t j = i + 1; j < n; ++j, ++k) {
      id1[k] = i + 1;
      id2[k] = j + 1;
      lat1[k] = x[i];
      lon1[k] = y[i];
      lat2[k] = x[j];
      lon2[k] = y[j];
    }
  }
  return List::create(id1, id2, lat1, lon1, lat2, lon2);
}

// [[Rcpp::export(rng = false)]]
List Z4P(DoubleVector x, DoubleVector y, int nThread = 1) {
  R_xlen_t n = x.length();
  R_xlen_t N = sum_n_le2(n);
  IntegerVector id1 = no_init(N);
  IntegerVector id2 = no_init(N);
  DoubleVector lat1 = no_init(N);
  DoubleVector lat2 = no_init(N);
  DoubleVector lon1 = no_init(N);
  DoubleVector lon2 = no_init(N);
  
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
  for (R_xlen_t i = 0; i < n; ++i) {
    R_xlen_t k0 = sum_n_minus_seq(i, n);
    for (R_xlen_t j = i + 1; j < n; ++j) {
      R_xlen_t k = k0 + j - i - 1;
      id1[k] = i + 1;
      id2[k] = j + 1;
      lat1[k] = x[i];
      lon1[k] = y[i];
      lat2[k] = x[j];
      lon2[k] = y[j];
    }
  }
  return List::create(id1, id2, lat1, lon1, lat2, lon2);
}

