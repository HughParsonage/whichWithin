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

SEXP Ctest_sum_identities(SEXP kk, SEXP jj, SEXP NN) {
  int k = asInteger(kk),  j = asInteger(jj), N = asInteger(NN);
  SEXP oout = allocVector(INTSXP, 4);
  int * out = INTEGER(oout);
  out[0] = sum_first_k(k);
  out[1] = sum_n_minus_seq(j, N);
  out[2] = sum_n_le(N);
  out[3] = sum_n_le2(N);
  return oout;
}


SEXP Z4(SEXP xx, SEXP yy) {
  if (!isReal(xx) || !isReal(yy)) {
    error("xx and yy must be REALSXP."); // # nocov
  }
  const double * x = REAL(xx);
  const double * y = REAL(yy);
  R_xlen_t n = xlength(xx);
  R_xlen_t N = sum_n_le2(n);
  SEXP iid1 = PROTECT(allocVector(INTSXP, N));
  SEXP iid2 = PROTECT(allocVector(INTSXP, N));
  SEXP llat1 = PROTECT(allocVector(REALSXP, N));
  SEXP llat2 = PROTECT(allocVector(REALSXP, N));
  SEXP llon1 = PROTECT(allocVector(REALSXP, N));
  SEXP llon2 = PROTECT(allocVector(REALSXP, N));
  
  int * restrict id1 = INTEGER(iid1);
  int * restrict id2 = INTEGER(iid2);
  double * restrict lat1 = REAL(llat1);
  double * restrict lat2 = REAL(llat2);
  double * restrict lon1 = REAL(llon1);
  double * restrict lon2 = REAL(llon2);
  
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
  SEXP ans = PROTECT(allocVector(VECSXP, 6));
  SET_VECTOR_ELT(ans, 0, iid1);
  SET_VECTOR_ELT(ans, 1, iid2);
  SET_VECTOR_ELT(ans, 2, llat1);
  SET_VECTOR_ELT(ans, 3, llon1);
  SET_VECTOR_ELT(ans, 4, llat2);
  SET_VECTOR_ELT(ans, 5, llon2);
  UNPROTECT(7);
  return ans;
}


SEXP Z4P(SEXP xx, SEXP yy, SEXP nthreads) {
  int nThread = asInteger(nthreads);
  if (!isReal(xx) || !isReal(yy)) {
    error("xx and yy must be REALSXP."); // # nocov
  }
  const double * x = REAL(xx);
  const double * y = REAL(yy);
  R_xlen_t n = xlength(xx);
  R_xlen_t N = sum_n_le2(n);
  SEXP iid1 = PROTECT(allocVector(INTSXP, N));
  SEXP iid2 = PROTECT(allocVector(INTSXP, N));
  SEXP llat1 = PROTECT(allocVector(REALSXP, N));
  SEXP llat2 = PROTECT(allocVector(REALSXP, N));
  SEXP llon1 = PROTECT(allocVector(REALSXP, N));
  SEXP llon2 = PROTECT(allocVector(REALSXP, N));
  
  int * restrict id1 = INTEGER(iid1);
  int * restrict id2 = INTEGER(iid2);
  double * restrict lat1 = REAL(llat1);
  double * restrict lat2 = REAL(llat2);
  double * restrict lon1 = REAL(llon1);
  double * restrict lon2 = REAL(llon2);
  
  R_xlen_t k = 0;
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
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
  SEXP ans = PROTECT(allocVector(VECSXP, 6));
  SET_VECTOR_ELT(ans, 0, iid1);
  SET_VECTOR_ELT(ans, 1, iid2);
  SET_VECTOR_ELT(ans, 2, llat1);
  SET_VECTOR_ELT(ans, 3, llon1);
  SET_VECTOR_ELT(ans, 4, llat2);
  SET_VECTOR_ELT(ans, 5, llon2);
  UNPROTECT(7);
  return ans;
}

