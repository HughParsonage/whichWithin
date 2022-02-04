#include "whichWithin.h"

void verify_sorted2(R_xlen_t N, SEXP xx, SEXP yy, int err_no) {
  if (xlength(xx) != xlength(yy)) {
    error("(verify_sorted2): x and y had different lengths."); // # nocov
  }
  switch(TYPEOF(xx)) {
  case REALSXP:
    switch(TYPEOF(yy)) {
    case REALSXP: {
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
      break;
      
    }
  case INTSXP:
    switch(TYPEOF(yy)) {
    case INTSXP: {
      const int * x = INTEGER(xx);
      const int * y = INTEGER(yy);
      bool isnt_sorted2 = false;
      for (R_xlen_t i = 1; i < N; ++i) {
        int x0 = x[i - 1];
        int x1 = x[i];
        int y0 = y[i - 1];
        int y1 = y[i];
        isnt_sorted2 = 
          isnt_sorted2 ||
          (x0 > x1) ||
          (x0 == x1 && y0 > y1);
      }
      
      if (isnt_sorted2) {
        error("Error #%d, Internal error: wasn't sorted in x, y.", err_no);
      }
    }
      
    }
  }
}


void verify_sorted_int(R_xlen_t N, const int * xp, const char * nx) {
  if (N <= 1) {
    return;
  }
  for (R_xlen_t i = 1; i < N; ++i) {
    if (xp[i - 1] > xp[i]) {
      error("`%s` was sorted at position, i = %lld", nx, i);
    }
  }
  
}
