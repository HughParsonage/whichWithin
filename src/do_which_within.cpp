#include "whichWithin.h"

// [[Rcpp::export(rng = false)]]
int engrid_1D(double x, double r, double xmin, double Rx) {
  return (x - xmin) / r;
}



// [[Rcpp::export(rng = false)]]
List do_which_within(DoubleVector lat, DoubleVector lon, double r, double lambda0) {
  R_xlen_t N = lat.length();
  if (N != lon.length() || N <= 1 || N > INT_MAX) {
    stop("Internal error(do_which_within): bad lengths."); // # nocov
  }
  verify_sorted2(N, lat, lon, DO_WHICH_WITHIN_SORTED2_ERR_NO);
  
  double ranlatlon[4] = {lat[0],
                         lat[N - 1],
                            lon[0],
                               lon[N - 1]};
  
  DoubleVector x = no_init(N);
  DoubleVector y = no_init(N);
  sinusoidal(N, x, y, lat, lon, lambda0);
  
  double cart_r = r / EARTH_RADIUS_KM;
  
  std::vector<int> orig;
  orig.reserve(N);
  std::vector<int> dest;
  dest.reserve(N);
  
  double R2 = cart_r * cart_r;
  
  // for engrid_1D -- want max/max < 1. 
  // Note that putting nextafter within engrid_1D will _double_ the runtime
  cart_r = std::nextafter(cart_r, R_PosInf); 
  
  double ymin = y[0];
  double ymax = y[N - 1];
  double xminmax[2] = {x[0], x[N - 1]};
  aminmax1(xminmax, x, N);
  double xmin = xminmax[0];
  double xmax = xminmax[1];
  
  for (R_xlen_t i = 0; i < N - 1; ++i) {
    double xi = x[i];
    double yi = y[i];
    int gix = engrid_1D(xi, cart_r, xmin, xmax);
    int giy = engrid_1D(yi, cart_r, ymin, ymax);
    
    for (R_xlen_t j = (i + 1); j < N; ++j) {
      double xj = x[j];
      double yj = y[j];
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
      double d2 = euclid_dist_sq(x[i], y[i], x[j], y[j]);
      if (d2 < R2) {
        orig.push_back(i + 1);
        dest.push_back(j + 1);
      }
    }
  }
  
  return List::create(Named("orig") = wrap(orig), Named("dest") = wrap(dest));
}

// [[Rcpp::export(rng = false)]]
LogicalVector do_is_within2(DoubleVector lat, DoubleVector lon, double r, double lambda0) {
  R_xlen_t N = lat.length();
  if (N != lon.length() || N <= 1 || N > INT_MAX) {
    stop("Internal error(do_which_within): bad lengths.");
  }
  verify_sorted2(N, lat, lon, DO_WHICH_WITHIN_SORTED2_ERR_NO);
  
  double ranlatlon[4] = {lat[0],
                         lat[N - 1],
                            lon[0],
                               lon[N - 1]};
  
  DoubleVector x = no_init(N);
  DoubleVector y = no_init(N);
  sinusoidal(N, x, y, lat, lon, lambda0);
  
  double cart_r = r / EARTH_RADIUS_KM;
  
  LogicalVector out(N);
  
  double R2 = cart_r * cart_r;
  
  // for engrid_1D -- want max/max < 1. 
  // Note that putting nextafter within engrid_1D will _double_ the runtime
  cart_r = std::nextafter(cart_r, R_PosInf); 
  
  double ymin = y[0];
  double ymax = y[N - 1];
  double xminmax[2] = {x[0], x[N - 1]};
  aminmax1(xminmax, x, N);
  double xmin = xminmax[0];
  double xmax = xminmax[1];
  
  for (R_xlen_t i = 0; i < N - 1; ++i) {
    double xi = x[i];
    double yi = y[i];
    int gix = engrid_1D(xi, cart_r, xmin, xmax);
    int giy = engrid_1D(yi, cart_r, ymin, ymax);
    
    for (R_xlen_t j = (i + 1); j < N; ++j) {
      double xj = x[j];
      double yj = y[j];
      int gjx = engrid_1D(xj, cart_r, xmin, xmax);
      
      int dgijx = gjx - gix;
      
      if (dgijx && dgijx != 1) {
        continue;
      }
      int gjy = engrid_1D(yj, cart_r, ymin, ymax);
      int dgijy = gjy - giy;
      if (dgijy > 1) {
        break;
      }
      double d2 = euclid_dist_sq(x[i], y[i], x[j], y[j]);
      if (d2 < R2) {
        out[i] = TRUE;
        out[j] = TRUE;
      }
    }
  }
  return out;
}


// [[Rcpp::export(rng = false)]]
List do_which_within_within_1km(DoubleVector lat, DoubleVector lon) {
  int N = lat.length();
  std::vector<int> orig;
  orig.reserve(N);
  std::vector<int> dest;
  dest.reserve(N);
  
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
        orig.push_back(i + 1);
        dest.push_back(j + 1);
      }
    }
  }
  
  return List::create(Named("orig") = wrap(orig), Named("dest") = wrap(dest));
}

// [[Rcpp::export(rng = false)]]
LogicalVector do_is_within(DoubleVector lat, DoubleVector lon, double r, bool debug = false) {
  R_xlen_t N = lat.length();
  
  verify_sorted2(N, lat, lon, 0);
  
  LogicalVector out(N);
  
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
  return out;
}

void cap_at_2(unsigned char & x) {
  x = (x > 1) ? 2 : 1;
}

// [[Rcpp::export(rng = false)]]
LogicalVector is_within_pixels(DoubleVector lat, DoubleVector lon, double r, double lambda0) {
  R_xlen_t N = lat.length();
  if (N != lon.length() || N <= 1 || N > INT_MAX) {
    stop("Internal error(do_which_within): bad lengths.");
  }
  verify_sorted2(N, lat, lon, DO_WHICH_WITHIN_SORTED2_ERR_NO);
  if (r < 0.01) {
    stop("r < 0.01 not supported.");
  }
  
  double ranlatlon[4] = {lat[0],
                         lat[N - 1],
                            lon[0],
                               lon[N - 1]};
  
  DoubleVector x = no_init(N);
  DoubleVector y = no_init(N);
  sinusoidal(N, x, y, lat, lon, lambda0);
  
  double cart_r = r / EARTH_RADIUS_KM;
  
  double R2 = cart_r * cart_r;
  
  // for engrid_1D -- want max/max < 1. 
  // Note that putting nextafter within engrid_1D will _double_ the runtime
  cart_r = std::nextafter(cart_r, R_PosInf); 
  
  double ymin = y[0];
  double ymax = y[N - 1];
  double xminmax[2] = {x[0], x[N - 1]};
  aminmax1(xminmax, x, N);
  double xmin = xminmax[0];
  double xmax = xminmax[1];
  
  unsigned char gg[GG_RES][GG_RES] = {};
  
  // Must make sure that engrid_1D won't exceed 2047
  double max_gx = (xmax - xmin) / cart_r;
  double max_gy = (ymax - ymin) / cart_r;
  if (max_gx > GG_RES || max_gy > GG_RES) {
    Rcerr << "max_gx = " << max_gx << ", " << "max_gy  = " << max_gy << "\n";
    Rcerr << "GG_REX = " << GG_RES << "\n";
    stop("max_gx exceeded. (radius too small for pixels).");
  }
  
  
  
  
  IntegerVector GX = no_init(N);
  IntegerVector GY = no_init(N);
  
  // Count number of entries in a particular 'pixel'
  // if more than two then certainly not isolated
  for (R_xlen_t i = 0; i < N; ++i) {
    double xi = x[i];
    double yi = y[i];
    int gix = engrid_1D(xi, cart_r, xmin, xmax);
    int giy = engrid_1D(yi, cart_r, ymin, ymax);
    GX[i] = gix;
    GY[i] = giy;
    
    cap_at_2(gg[gix][giy]);
  }
  
  
  LogicalVector out(N);
  
  for (R_xlen_t i = 0; i < N; ++i) {
    int gix = GX[i];
    int giy = GY[i];
    if (gg[gix][giy] >= 2) {
      out[i] = TRUE;
      continue;
    }
    
    double xi = x[i];
    double yi = y[i];

    
    for (R_xlen_t j = (i + 1); j < N; ++j) {
      double xj = x[j];
      double yj = y[j];
      int gjx = GX[j];
      int gjy = GY[j];

      int dgijx = gjx - gix;

      if (dgijx > 1) {
        continue;
      }
      int dgijy = gjy - giy;
      if (dgijy > 1) {
        break;
      }
      double d2 = euclid_dist_sq(x[i], y[i], x[j], y[j]);
      if (d2 < R2) {
        out[i] = TRUE;
        out[j] = TRUE;
      }
    }
  }
  return out;
  
}



