#include "whichWithin.h"

SEXP outer_1ds_R(const int * xp,
                 int N,
                 const int r0,
                 const double ion) {
  SEXP ans = PROTECT(allocVector(INTSXP, N));
  int * restrict ansp = INTEGER(ans);
  memset(ansp, 0, sizeof(int) * N);
  int k = 1;
  for (int i = 0; i < N - 1; ++i) {
    int xpi = xp[i];
    if (i >= 1 && xp[i - 1] == xpi) {
      ansp[i] = ansp[i - 1];
      continue;
    }
    for (int j = k; j < N; ++j) {
      int xpj = xp[j];
      int d_ji = xpj - xpi;
      if (d_ji > r0) {
        break;
      }
      if (d_ji < r0) {
        ansp[i] = j + 1;
        k = j + 1;
      }
    }
  }
  UNPROTECT(1);
  return ans;
}

SEXP outer2List(SEXP x, SEXP Outer) {
  checkEquiIntInt(x, Outer, "x", "Outer");
  R_xlen_t N = xlength(x);
  const int * xp = INTEGER(x);
  const int * outer = INTEGER(Outer);
  R_xlen_t oN = 0;
  for (R_xlen_t i = 0; i < N; ++i) {
    oN += outer[i];
  }
  bool err_message = true;
  
  SEXP orig = PROTECT(allocVector(INTSXP, oN));
  SEXP dest = PROTECT(allocVector(INTSXP, oN));
  int * origp = INTEGER(orig);
  int * destp = INTEGER(dest);
  R_xlen_t k = 0;
  for (R_xlen_t i = 0; i < N; ++i) {
    R_xlen_t M = outer[i];
    int oi = xp[i];
    for (R_xlen_t j = i + 1; j < M; ++j) {
      if (err_message && j >= oN) {
        Rprintf("i = %lld, j = %lld, k = %lld, oN = %lld\n", i, j, k, oN);
        err_message = false;
      }
      int xpj = xp[j];
      origp[k] = oi;
      destp[k] = xpj;
      ++k;
    }
  }
  SEXP ans = PROTECT(allocVector(VECSXP, 2));
  SET_VECTOR_ELT(ans, 0, orig);
  SET_VECTOR_ELT(ans, 2, dest);
  UNPROTECT(3);
  return ans;
}


SEXP which_within1ds_R1(const int * xp, int N, const int r0, const double ion) {
  if (N <= 1) {
    return IntegerN(0);
  }
  uint64_t oN = ion * ((uint64_t)N);
  int * orig = malloc(sizeof(int) * oN);
  if (orig == NULL) {
    free(orig);
    error("dest could not be malloc'd: %" PRIu64" bytes", oN); // # nocov
  }
  int * dest = malloc(sizeof(int) * oN);
  if (dest == NULL) {
    free(orig);
    free(dest);
    error("dest could not be malloc'd: %" PRIu64" bytes", oN); // # nocov
  }
  
  memset(orig, 0, sizeof(int) * oN);
  memset(dest, 0, sizeof(int) * oN);
  const bool desc = xp[0] > xp[N - 1];
  
  R_xlen_t k = 0;
  if (desc) {
    for (int i = 0; i < N; ++i) {
      if ((k + N + 32) >= oN) {
        oN += (oN >> 1) + (oN >> 3); // * ~1.62 
        orig = realloc(orig, sizeof(int) * oN);
        dest = realloc(dest, sizeof(int) * oN);
        if (orig == NULL || dest == NULL) {
          free(orig);
          free(dest);
          error("Unable to realloc orig and dest"); // # nocov
        }
      }
      int xpi = xp[i];
      for (int j = i + 1; j < N; ++j) {
        int xpj = xp[j];
        // desc
        int d_ji = xpi - xpj; 
        if (d_ji > r0) {
          break;
        }
        if (d_ji < r0) {
          orig[k] = i + 1;
          dest[k] = j + 1;
          ++k;
        }
      }
    }
  } else {
    for (int i = 0; i < N; ++i) {
      if ((k + N + 32) >= oN) {
        Rprintf("=====================================\n");
        oN += (oN >> 1) + (oN >> 3); // * ~1.62 
        orig = realloc(orig, sizeof(int) * oN);
        dest = realloc(dest, sizeof(int) * oN);
        if (orig == NULL || dest == NULL) {
          free(orig);
          free(dest);
          error("Unable to realloc orig and dest"); // # nocov
        }
      }
      int xpi = xp[i];
      for (int j = i + 1; j < N; ++j) {
        int xpj = xp[j];
        // ascending
        int d_ji = xpj - xpi; 
        if (d_ji > r0) {
          break;
        }
        if (d_ji < r0) {
          orig[k] = i + 1;
          dest[k] = j + 1;
          ++k;
        }
      }
    }
  }
  SEXP ans = PROTECT(allocVector(VECSXP, 2));
  SEXP ans0 = PROTECT(allocVector(INTSXP, k));
  SEXP ans1 = PROTECT(allocVector(INTSXP, k));
  int * ans0p = INTEGER(ans0);
  int * ans1p = INTEGER(ans1);
  for (R_xlen_t i = 0; i < k; ++i) {
    ans0p[i] = orig[i];
    ans1p[i] = dest[i];
  }
  
  free(orig);
  free(dest);
  SET_VECTOR_ELT(ans, 0, ans0);
  SET_VECTOR_ELT(ans, 1, ans1);
  UNPROTECT(3);
  return ans;
}

SEXP Cwhich_within1d_R(SEXP x, SEXP R, SEXP Ion) {
  if (!isInteger(x) || !isInteger(R)) {
    error("`x` was type '%s', and `R` was type '%s' but both must be integer.", 
          type2char(TYPEOF(x)), type2char(TYPEOF(R)));
  }
  if (!isReal(Ion)) {
    error("Ion must be a double."); // # nocov
  }
  int N = length(x);
  const int * xp = INTEGER(x);
  const double ion = asReal(Ion) < 1 ? 100 : asReal(Ion);
  
  const int r0 = asInteger(R);
  // const int r1 = (xlength(R) == 1) ? r0 : INTEGER_ELT(R, 1);
  
  if (is_sorted(x)) {
    if (xlength(R) == 2) {
      return R_NilValue;// which_within1ds_R2(xp, N, r0, r1);
    } else {
      // return which_within1ds_R1(xp, N, r0, ion);  
      return outer_1ds_R(xp, N, r0, ion);
    }
    
    
  }
  
  R_xlen_t oN = N * 100;
  int * orig = calloc(oN, sizeof(int));
  int * dest = calloc(oN, sizeof(int));
  if (orig == NULL || dest == NULL) {
    free(orig);
    free(dest);
    error("orig,dest could not be calloc'd"); // # nocov
  }
  
  R_xlen_t k = 0;
  for (int i = 0; i < N - 1; ++i) {
    if (k + N >= oN) {
      oN += (oN >> 3) + (oN >> 1);
      int * new_orig = realloc(orig, sizeof(int) * oN);
      if (new_orig == NULL) {
        free(orig);
        error("orig could not realloc'd");
      }
      orig = new_orig;
      int * new_dest = realloc(dest, sizeof(int) * oN);
      if (new_dest == NULL) {
        free(orig);
        free(dest);
        error("dest could not realloc'd");
      }
      dest = new_dest;
    }
    int xpi = xp[i];
    for (int j = i + 1; j < N; ++j) {
      int xpj = xp[j];
      int dpj = xpj > xpi ? xpj - xpi : xpi - xpj;
      if (dpj <= r0) {
        orig[k] = i + 1;
        dest[k] = j + 1;
        ++k;
      }
    }
  }
  SEXP ans = PROTECT(allocVector(VECSXP, 2));
  SEXP ans0 = PROTECT(allocVector(INTSXP, k));
  SEXP ans1 = PROTECT(allocVector(INTSXP, k));
  int * ans0p = INTEGER(ans0);
  int * ans1p = INTEGER(ans1);
  for (R_xlen_t i = 0; i < k; ++i) {
    ans0p[i] = orig[i];
    ans1p[i] = dest[i];
  }
  
  free(orig);
  free(dest);
  SET_VECTOR_ELT(ans, 0, ans0);
  SET_VECTOR_ELT(ans, 1, ans1);
  UNPROTECT(3);
  return ans;
}

SEXP Cmerge2int(SEXP E, SEXP Ewidth, SEXP Time, SEXP Tradius) {
  // Take an gridpoint e (with a max Ewidth)
  // together with a time and a tradius
  // return 
  
  checkEquiIntInt(E, Time, "E", "Time");
  checkEquiIntInt(Ewidth, Tradius, "Ewidth", "Tradius");
  
  R_xlen_t xN = xlength(E);
  
  const int * ep = INTEGER(E);
  const int * tp = INTEGER(Time);
  verify_sorted_int(xN, tp, "Time");
  if (xN > INT_MAX) {
    error("(merge2int)Long vectors are not supported"); // # nocov
  }
  
  int N = (int)xN;
  
  const int ew = asInteger(Ewidth);
  const int tr = asInteger(Tradius);
  
  
  SEXP ans = PROTECT(allocVector(INTSXP, N));
  int * restrict ansp = INTEGER(ans);
  memset(ansp, 0, N * sizeof(int));
  
  for (int i = 0; i < N; ++i) {
    if (ansp[i]) {
      // already found
      continue;
    }
    int time_i = tp[i];
    int epi = ep[i] % ew;
    for (int j = i + 1; j < N; ++j) {
      int time_j = tp[j];
      if ((time_j - time_i) > tr) {
        break;
      }
      int epj = ep[j] % ew;
      if (epj < epi - 1) {
        continue;
      }
      if (epj <= epi + 1) {
        ansp[i] = j + 1;
        ansp[j] = i + 1;
        break;
      }
    }
  }
  
  UNPROTECT(1);
  return ans;
}

int cmpfunc(const void * a, const void * b) {
  return ( *(int*)a - *(int*)b );
}

bool binary_in(int key, int * xp, int N) {
  int * res = (int *)bsearch(&key, xp, N, sizeof(int), cmpfunc);
  if (res) {
    return true;
  }
  return false;
}

SEXP Capprox_dvr_matches(SEXP xCaseNumber,
                         SEXP Distance,
                         SEXP Duration,
                         SEXP CaseNumber, 
                         SEXP Lat, SEXP Lon,
                         SEXP VisitDateTime,
                         SEXP Option,
                         SEXP Ion) {
  const double r_d = asReal(Distance);
  const unsigned int r_t = asInteger(Duration);
  if (r_t > 1048576) {
    error("r_t(duration) > 1048576 this is an unlikely value for the number of seconds."); 
  }
  const unsigned int r2_t = r_t << 1;
  checkEquiIntInt(CaseNumber, VisitDateTime, "CaseNumber", "VisitDateTime");
  checkEquiRealReal(Lat, Lon, "Lat", "Lon");
  int N = length(CaseNumber);
  if (xlength(Lat) != N) {
    error("xlength(Lat) = %lld, yet xlength(CaseNumber) = %lld. Lengths must be equal.", 
          xlength(Lat), N);
  }
  int n_xCaseNumber = length(xCaseNumber);
  int * xp = INTEGER(xCaseNumber);  // # the case numbers requested
  const int * cno = INTEGER(CaseNumber);
  const int * vdt = INTEGER(VisitDateTime);
  const double * lat = REAL(Lat);
  const double * lon = REAL(Lon);
  
  double ion = asReal(Ion);
  if (ion <= 0) {
    ion = N;
  }
  if (N > 100 && ion <= 100) {
    ion *= N;
  }
  uint64_t oN = ion + 1;
  uint64_t k = 0;
  int * orig = calloc(oN, sizeof(int));
  int * dest = calloc(oN, sizeof(int));
  if (orig == NULL || dest == NULL) {
    free(orig);
    free(dest);
    error("Could not calloc orig/dest"); // # nocov
  }
  const double rx = 0.000011352150 * r_d;
  const double ry = 0.000009007702 * r_d;
  
  for (int i = 0; i < N - 1; ++i) {
    int cno_i = cno[i];
    double lati = lat[i];
    double loni = lon[i];
    if (!binary_in(cno_i, xp, n_xCaseNumber) ||
        ISNAN(lati) || ISNAN(loni)) {
      // ignore CaseNumbers
      continue;
    }
    
    // if (i >= 4544830 && i <= 4544832) Rprintf("%d", i);
    if (k + N >= (oN - 1)) {
      // if (k <= UINT_MAX) Rprintf("\n===> k = %u\n", (unsigned int)k);
      oN += (oN >> 1) + (oN >> 3); // * ~1.62 
      // Rprintf("(i = %d)orig realloc'd: %" PRIu64" bytes\n", i, oN);
      int * new_orig = (int * )realloc(orig, oN * sizeof(int));
      if (new_orig == NULL) {
        free(orig);
        free(dest);
        // Rprintf("(i = %d)orig could not be realloc'd: %" PRIu64" bytes", i, oN);
        error("(i = %d)orig could not be realloc'd: %" PRIu64" bytes", i, oN); // # nocov
      }
      orig = new_orig;
      int * new_dest = (int *)realloc(dest, oN * sizeof(int));
      // Rprintf("(i = %d)dest realloc'd: %" PRIu64" bytes\n", i, oN);
      if (new_dest == NULL) {
        free(orig);
        free(dest);
        // Rprintf("(i = %d)orig could not be realloc'd: %" PRIu64" bytes", i, oN);
        error("(i = %d)dest could not be realloc'd: %" PRIu64" bytes", i, oN); // # nocov
      }
      dest = new_dest;
      // Rprintf("complete");
    }
    unsigned int vdti = vdt[i];
    unsigned int vdt_lhs = vdti - r_t;
    
    bool location_match = false;
    if (lat[i + 1] == lati &&
        lon[i + 1] == loni) {
      int j = i + 1;
      // Same place; if we get a match, do not look at (expensive lat/lon operations)
      unsigned int vdtj_rel_lhs = ((unsigned int)vdt[j]) - vdt_lhs;
      if (vdtj_rel_lhs <= r2_t) {
        location_match = true;
        orig[k] = i + 1;
        dest[k] = j + 1;
        ++k;
        while (++j < N && lat[j] == lati && lon[j] == loni) {
          unsigned int vdtj_rel_lhs = ((unsigned int)vdt[j]) - vdt_lhs;
          if (vdtj_rel_lhs <= r2_t) {
            orig[k] = i + 1;
            dest[k] = j + 1;
            ++k;
          }
        }
        continue; // # no need to check non-matching lat/lons
      } else {
        while (++j < N && lat[j] == lati && lon[j] == loni) {
          unsigned int vdtj_rel_lhs = ((unsigned int)vdt[j]) - vdt_lhs;
          if (vdtj_rel_lhs <= r2_t) {
            location_match = true;
            orig[k] = i + 1;
            dest[k] = j + 1;
            ++k;
          }
        }
        if (location_match) {
          continue;
        }
      }
    }
    
    // TODO: 
    // Check whether the previous lat, lon is the same; if it is, we can copy
    // the previous entries
    double exlati = lati + ry;
    
    for (int j = i + 1; j < N; ++j) {
      double latj = lat[j];
      double lonj = lon[j];
      if (latj > exlati) {
        break;
      }
      if (fabs(lonj - loni) <= rx) {
        int vdtj = vdt[j];
        int dtij = (vdti < vdtj) ? (vdtj - vdti) : (vdti - vdtj);
        // if (i >= 4544830 && i <= 4544832) Rprintf("%d,", dtij);
        if (dtij <= r_t) {
          // if (i >= 4544830 && i <= 4544832) Rprintf("%d\n", j);
          // if (i >= 4544830 && i <= 4544832) Rprintf(".");
          
          // if (i >= 4544830 && i <= 4544832)Rprintf("(i = %d)k: %" PRIu64" | ", i, k);
          // if (i >= 4544830 && i <= 4544832)Rprintf("orig[k] = ");
          
          // if (i >= 4544830 && i <= 4544832)Rprintf("%d <- %d ~ ", origk, i);
          orig[k] = i + 1;
          // if (i >= 4544830 && i <= 4544832) Rprintf("(j = %d)k: %" PRIu64" | ", j, k);
          dest[k] = j + 1;
          ++k;
        }
      }
    }
    
    
  }
  
  SEXP ans = PROTECT(allocVector(VECSXP, 2));
  SEXP ans0 = PROTECT(allocVector(INTSXP, k));
  SEXP ans1 = PROTECT(allocVector(INTSXP, k));
  int * ans0p = INTEGER(ans0);
  int * ans1p = INTEGER(ans1);
  for (R_xlen_t i = 0; i < k; ++i) {
    ans0p[i] = orig[i];
    ans1p[i] = dest[i];
  }
  
  free(orig);
  free(dest);
  SET_VECTOR_ELT(ans, 0, ans0);
  SET_VECTOR_ELT(ans, 1, ans1);
  UNPROTECT(3);
  return ans;
  
}







