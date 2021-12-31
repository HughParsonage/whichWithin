#include "whichWithin.h"


int engrid_1D(double x, double r, double xmin, double Rx) {
  return (x - xmin) / r;
}

int array2(int i, int j, int imax, int jmax) {
  return i + j * imax;
}


SEXP do_which_within(SEXP llat, SEXP llon, SEXP rr, 
                     SEXP llambda0, 
                     SEXP iincl_distance) {
  const double r = asReal(rr);
  const double lambda0 = asReal(llambda0);
  const bool incl_distance = asLogical(iincl_distance);
  R_xlen_t N = xlength(llat);
  
  if (N != xlength(llon) || N <= 1 || N > INT_MAX) {
    error("Internal error(do_which_within): bad lengths."); // # nocov
  }
  const double * lat = REAL(llat);
  const double * lon = REAL(llon);
  verify_sorted2(N, llat, llon, DO_WHICH_WITHIN_SORTED2_ERR_NO);
  
  SEXP x = PROTECT(allocVector(REALSXP, N));
  SEXP y = PROTECT(allocVector(REALSXP, N));
  double * xp = REAL(x);
  double * yp = REAL(y);
  sinusoidal(N, xp, yp, lat, lon, lambda0);
  
  double cart_r = r / EARTH_RADIUS_KM;
  
  R_xlen_t oN = 0;
  uint64_t oM = N * 200; // # by observation
  int * orig = malloc(sizeof(int) * oM);
  int * dest = malloc(sizeof(int) * oM);
  double * out_dist = malloc(sizeof(double) * (incl_distance ? oM : 1));
  if (orig == NULL || dest == NULL || out_dist == NULL) {
    UNPROTECT(2);
    free(orig);
    free(dest);
    free(out_dist);
    error("Unable to allocate orig,dest,out_dist via malloc."); // # nocov
  }
  
  double R2 = cart_r * cart_r;
  
  // for engrid_1D -- want max/max < 1. 
  // Note that putting nextafter within engrid_1D will _double_ the runtime
  cart_r = nextafter(cart_r, R_PosInf); 
  
  double ymin = REAL(y)[0];
  double ymax = REAL(y)[N - 1];
  double xminmax[2] = {REAL(x)[0], REAL(x)[N - 1]};
  aminmax1(xminmax, xp, N);
  double xmin = xminmax[0];
  double xmax = xminmax[1];
  
  for (R_xlen_t i = 0; i < N - 1; ++i) {
    if ((oN + N) >= oM) {
      // Originally this was oM += N  but this was poor
      // 11s for 200,000 vs 4.6s for Rcpp push_back.
      // After this was changed to *= 1.62, inspired by
      // https://stackoverflow.com/questions/1100311/what-is-the-ideal-growth-rate-for-a-dynamically-allocated-array
      // we had similar performance 4.5s
      oM += (oM >> 1) + (oM >> 3); // * ~1.62 
      orig = realloc(orig, sizeof(int) * oM);
      dest = realloc(dest, sizeof(int) * oM);
      if (incl_distance) {
        out_dist = realloc(out_dist, sizeof(double) * oM);
      }
      if (orig == NULL || dest == NULL || out_dist == NULL) {
        free(orig);
        free(dest);
        free(out_dist);
        error("Unable to realloc %" PRIu64" bytes", oM); // # nocov
      }
    }
    double xi = xp[i];
    double yi = yp[i];
    int gix = engrid_1D(xi, cart_r, xmin, xmax);
    int giy = engrid_1D(yi, cart_r, ymin, ymax);
    
    for (R_xlen_t j = (i + 1); j < N; ++j) {
      double xj = xp[j];
      double yj = yp[j];
      int gjx = engrid_1D(xj, cart_r, xmin, xmax);
      int gjy = engrid_1D(yj, cart_r, ymin, ymax);
      int dgijx = gjx - gix;
      int dgijy = gjy - giy;
      if (dgijx > 1) {
        continue;
      }
      if (dgijy > 1) {
        break;
      }
      double d2 = euclid_dist_sq(xp[i], yp[i], xp[j], yp[j]);
      if (d2 < R2) {
        orig[oN] = i + 1;
        dest[oN] = j + 1;
        if (incl_distance) {
          double d = sqrt(d2);
          d *= EARTH_RADIUS_KM;
          out_dist[oN] = d;
        }
        ++oN;
      }
    }
  }
  
  // return List::create(Named("orig") = wrap(orig),
  //                     Named("dest") = wrap(dest),
  //                     Named("dist") = wrap(out_dist));
  SEXP ans = PROTECT(allocVector(VECSXP, 3));
  SEXP ans0 = PROTECT(allocVector(INTSXP, oN));
  SEXP ans1 = PROTECT(allocVector(INTSXP, oN));
  SEXP ans2 = PROTECT(allocVector(REALSXP, incl_distance ? oN : 1));
  int * ans0p = INTEGER(ans0);
  int * ans1p = INTEGER(ans1);
  double * ans2p = REAL(ans2);
  ans2p[0] = out_dist[0];
  for (R_xlen_t i = 0; i < oN; ++i) {
    ans0p[i] = orig[i];
    ans1p[i] = dest[i];
    if (incl_distance) {
      ans2p[i] = out_dist[i];
    }
  }
  free(orig);
  free(dest);
  free(out_dist);
  
  
  SET_VECTOR_ELT(ans, 0, ans0);
  SET_VECTOR_ELT(ans, 1, ans1);
  SET_VECTOR_ELT(ans, 2, ans2);
  UNPROTECT(6);
  return ans;
}



SEXP do_which_within_within_1km(SEXP llat, SEXP llon) {
  checkEquiRealReal(llat, llon, "lat", "lon");
  const double * lat = REAL(llat);
  const double * lon = REAL(llon);
  int N = length(llat);
  int * orig = malloc(sizeof(int) * N);
  int * dest = malloc(sizeof(int) * N);
  if (orig == NULL || dest == NULL) {
    error("orig and dest could not be malloc"); // # nocov
  }
  R_xlen_t k = 0;
  R_xlen_t oN = N;
  for (int i = 0; i < N; ++i) {
    double olat1 = lat[i];
    double olon1 = lon[i];
    for (int j = i + 1; j < N; ++j) {
      double olat2 = lat[j];
      double olon2 = lon[j];
      int d_lat = 150 * (olat2 - olat1);
      int d_lon = 150 * (olon2 - olon1);
      if (d_lat & d_lon) {
        if (d_lat > 1 && d_lon > 1) {
          break;
        }
        continue;
      }
      
      if (haversine_dist(olat1, olon1, olat2, olon2) < 1) {
        orig[k] = i + 1;
        dest[k] = j + 1;
        ++k;
        if ((k * 2) >= oN) {
          oN *= 2;
          orig = realloc(orig, sizeof(int) * oN);
          dest = realloc(dest, sizeof(int) * oN);
        }
        
      }
    }
  }
  SEXP ans = PROTECT(allocVector(VECSXP, 2));
  SEXP ans1 = PROTECT(allocVector(INTSXP, k));
  SEXP ans2 = PROTECT(allocVector(INTSXP, k));
  SET_VECTOR_ELT(ans, 0, ans1);
  SET_VECTOR_ELT(ans, 1, ans2);
  UNPROTECT(3);
  
  // return List::create(Named("orig") = wrap(orig), Named("dest") = wrap(dest));
  return ans;
}


SEXP Cdo_is_within(SEXP llat, SEXP llon, SEXP rr) {
  checkEquiRealReal(llat, llon, "lat", "lon");
  R_xlen_t N = xlength(llat);
  const double * lat = REAL(llat);
  const double * lon = REAL(llon);
  double r = asReal(rr);
  
  verify_sorted2(N, llat, llon, 0);
  
  SEXP oout = PROTECT(allocVector(LGLSXP, N));
  int * out = LOGICAL(oout);
  memset(out, 0, sizeof(int) * N);
  
  double R = sin(r / (2 * EARTH_RADIUS_KM));
  R *= R;
  
  const double delta_lati = ((r / EARTH_RADIUS_KM));
  
  for (R_xlen_t i = 0; i < N; ++i) {
    if (!out[i] || true) {
      double lati = lat[i] * (M_PI / 180);
      double loni = lon[i] * (M_PI / 180);
      double coslat = cos(lati);
      double max_lati = lati + delta_lati;
      double min_lati = lati - delta_lati;
      
      for (R_xlen_t j = i + 1; j < N; ++j) {
        double lonj = lon[j] * (M_PI / 180);
        double latj = lat[j] * (M_PI / 180);
        if (latj > max_lati) {
          break;
        }
        if (latj < min_lati) {
          continue;
        }
        if (haversine_dist_klatlon(lati, loni, latj, lonj, coslat) < R) {
          // if (haversine_dist(lat[i], lon[i], lat[j], lon[j]) < r) {
          out[i] = true;
          out[j] = true;
        } else if (latj > max_lati) {
          break;
        } 
      }
    }
  }
  UNPROTECT(1);
  return oout;
}

void cap_at_2(unsigned char * x) {
  *x = (*x > 1) ? 2 : 1;
}

SEXP is_within_pixels(SEXP llat, SEXP llon, SEXP rr, SEXP llambda0) {
  checkEquiRealReal(llat, llon, "lat", "lon");
  checkEquiRealReal(rr, llambda0, "r", "lambda0");
  R_xlen_t N = xlength(llat);
  if (N != xlength(llon) || N <= 1 || N > INT_MAX) {
    error("Internal error(do_which_within): bad lengths.");
  }
  verify_sorted2(N, llat, llon, DO_WHICH_WITHIN_SORTED2_ERR_NO);
  const double * lat = REAL(llat);
  const double * lon = REAL(llon);
  double r = asReal(rr);
  double lambda0 = asReal(llambda0);
  if (r < 0.01) {
    error("r < 0.01 not supported.");
  }
  int np = 0;
  SEXP x = PROTECT(allocVector(REALSXP, N)); ++np;
  SEXP y = PROTECT(allocVector(REALSXP, N)); ++np;
  double * xp = REAL(x);
  double * yp = REAL(y);
  double lambda_0 = lambda0 + 0.0;
  if (ISNAN(lambda0)) {
    lambda_0 = lon[0];
    double n = (double)N;
    for (R_xlen_t i = 1; i < N; ++i) {
      if (ISNAN(lon[i])) {
        n -= 1;
      } else {
        lambda_0 += lon[i];
      }
    }
    lambda_0 /= n;
  }
  
  double radf = M_PI / 180.0;
  
  for (R_xlen_t i = 0; i < N; ++i) {
    if (ISNAN(lat[i]) || ISNAN(lon[i])) {
      xp[i] = R_PosInf;
      yp[i] = R_NegInf;
      continue;
    }
    yp[i] = lat[i] * radf;
    xp[i] = (lon[i] - lambda_0) * radf;
    double cos_phi = cos(yp[i]);
    xp[i] *= cos_phi;
  }
  
  double cart_r = r / EARTH_RADIUS_KM;
  
  double R2 = cart_r * cart_r;
  
  // for engrid_1D -- want max/max < 1. 
  // Note that putting nextafter within engrid_1D will _double_ the runtime
  cart_r = nextafter(cart_r / 2.0, R_PosInf); 
  
  double ymin = yp[0];
  double ymax = yp[N - 1];
  double xminmax[2] = {xp[0], xp[N - 1]};
  aminmax1(xminmax, xp, N);
  double xmin = xminmax[0];
  double xmax = xminmax[1];
  
  // Must make sure that engrid_1D won't exceed 2047
  double dmax_gx = (xmax - xmin) / cart_r;
  double dmax_gy = (ymax - ymin) / cart_r;
  int max_gx = (int)dmax_gx;
  int max_gy = (int)dmax_gy; 
  
  R_xlen_t gg_N = max_gx * max_gy + max_gx + max_gy + 1;
  unsigned char * gg = calloc(gg_N, sizeof(char));
  if (gg == NULL) {
    free(gg);
    UNPROTECT(np);
    error("gg could not be calloc'd");
  }
  SEXP GGX = PROTECT(allocVector(INTSXP, N)); np++;
  SEXP GGY = PROTECT(allocVector(INTSXP, N)); np++;
  int * restrict GX = INTEGER(GGX);
  int * restrict GY = INTEGER(GGY);
  
  // Count number of entries in a particular 'pixel'
  // if more than two then certainly not isolated
  for (R_xlen_t i = 0; i < N; ++i) {
    double xi = xp[i];
    double yi = yp[i];
    int gix = engrid_1D(xi, cart_r, xmin, xmax);
    int giy = engrid_1D(yi, cart_r, ymin, ymax);
    GX[i] = gix;
    GY[i] = giy;
    
    int ai = array2(gix, giy, max_gx, max_gy);
    unsigned char prev_ggai = gg[ai];
    
    gg[ai] = (prev_ggai >= 1) ? 2 : 1;
  }
  
  SEXP oout = PROTECT(allocVector(LGLSXP, N)); np++;
  int * restrict out = INTEGER(oout);
  memset(out, 0, sizeof(int) * N);
  for (R_xlen_t i = 0; i < N; ++i) {
    int gix = GX[i];
    int giy = GY[i];
    int ai = array2(gix, giy, max_gx, max_gy);
    if (gg[ai] >= 2) {
      out[i] = 1;
      // continue;  <-- not correct because the i'th
      //                point may have neighbours for
      //                whom i is the nearest point but 
      //                who are outside the pixel
      
      //  +-----+-----+
      //  |     |     |
      //  |     | j   |
      //  +-----+-----+
      //  |     |     |
      //  |     |     |
      //  +-----+-----+
      //  ^__
      //    i
      
    }
    
    double xi = xp[i];
    double yi = yp[i];
    
    for (R_xlen_t j = (i + 1); j < N; ++j) {
      int gjx = GX[j];
      int gjy = GY[j];

      int dgijx = gjx - gix;

      if (dgijx > 2) {
        continue;
      }
      int dgijy = gjy - giy;
      if (dgijy > 2) {
        break;
      }
      
      double xj = xp[j];
      double yj = yp[j];
      double d2 = euclid_dist_sq(xi, yi, xj, yj);
      if (d2 < R2) {
        out[i] = 1;
        out[j] = 1;
      }
    }
  }
  free(gg);
  UNPROTECT(np);
  return oout;
}


R_xlen_t locate_in_sorted(int xi, const int * y, R_xlen_t a, R_xlen_t b, R_xlen_t N) {
  // return the 'best' guess for the location of xi in y, sorted ascending
  // i.e. y[out] = xi if xi in y and y[out] != xi otherwise.
  if (xi <= y[a]) {
    return a;
  }
  
  R_xlen_t s = (b - a) / 4;
  if (s < 64) {
    for (R_xlen_t i = a; i <= b; ++i) {
      if (xi <= y[i]) {
        return i;
      }
    }
    return b;
  }
  R_xlen_t m1 = a + s;
  R_xlen_t m2 = m1 + s;
  R_xlen_t m3 = m2 + s;

  if (xi < m1) {
    return locate_in_sorted(xi, y, a, m1, N);
  }
  if (xi < m2) {
    return locate_in_sorted(xi, y, m1, m2, N);
  }
  if (xi < m3) {
    return locate_in_sorted(xi, y, m2, m3, N);
  }
  return locate_in_sorted(xi, y, m3, b, N);
}

R_xlen_t do_locate_in_sorted(int xi, SEXP yy) {
  R_xlen_t N = xlength(yy);
  const int * y = INTEGER(yy);
  R_xlen_t a = 0; 
  R_xlen_t b = N - 1;
  if (a < 0 || b < a || b >= N || xi >= y[b]) {
    return b;
  }
  return locate_in_sorted(xi, y, a, b, N);
}


bool all_in_sorted(SEXP xx, SEXP ttbl) {
  const int * x = INTEGER(xx);
  const int * tbl = INTEGER(ttbl);
  R_xlen_t N = xlength(xx);
  R_xlen_t tn = xlength(ttbl);
  if (tn == 0) {
    return false;
  }
  for (R_xlen_t i = 0; i < N; ++i) {
    int xi = x[i];
    for (R_xlen_t t = 0; t < tn; ++t) {
      if (xi == tbl[t]) {
        break;
      }
      if (xi < tbl[t] || (t + 1) == tn) {
        return false;
      }
    }
  }
  return true;
}

#if false
#include <omp.h>


SEXP Call_integers(SEXP nthreads) {
  int nThread = asInteger(nthreads)
  IntegerVector out = no_init(4294967296);
#pragma omp parallel for num_threads(nThread)
  for (unsigned int i = 0; i < 4294967295; ++i) {
    int outi = (i);
    out[i] = outi;
  }
  out[4294967295] = -1;
  return out;
}

#endif


