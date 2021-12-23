

compare_sinusoidal <- function(lat1, lon1, 
                               lat2, lon2,
                               use_geosphere = TRUE,
                               radii = c(1, 5, 10, 20)) {
  stopifnot(length(lat1) == length(lon1),
            length(lat2) == length(lon2))
  if (length(lat2) != length(lat1)) {
    if (length(lat2) != 1) {
      stop("`length(lat2) = ", length(lat2), "` but `length(lat1) = ", length(lat1), ".")
    }
    lat2 <- rep_len(lat2, length(lat1))
    lon2 <- rep_len(lon2, length(lon2))
  }
  d_sinusoidal <- dist_sinusoidal(lat1, lon1, lat2, lon2)
  if (use_geosphere) {
    d_geo <- dist_geo(lat1, lon1, lat2, lon2)
  } else {
    d_geo <- haversine_distance(lat1, lon1, lat2, lon2)
  }
  
  delta <- d_sinusoidal - d_geo
  abs_delta <- abs(delta)
  pct_delta <- delta / d_geo
  pct_delta <- pct_delta[and(lat1 != lat2, lon1 != lon2)]
  res <- data.table(lat1, lon1, lat2, lon2, d_sinusoidal, d_geo)
  class(res) <- c("small_data_table", class(res))
  
  n_geosphere <- integer(length(radii))
  n_sinusoidal <- integer(length(radii))
  for (i in seq_along(radii)) {
    r <- radii[i]
    n_geosphere[i] <- sum(d_geo < r)
    n_sinusoidal[i] <- sum(d_sinusoidal < r)
  }
  
  
  list(res = res,
       p_different_ge_100m_within_20k = mean(abs_delta[d_geo <= 20e3] > 0.01),
       p_incorrect_at_dists = data.table(radii, n_sinusoidal, n_geosphere, p = n_sinusoidal / n_geosphere - 1),
       p_different_ge_1m = mean(abs_delta > 0.001),
       p_different_ge_100m = mean(abs_delta > 0.01),
       p_different_ge_1pc = mean(abs(pct_delta) > 0.01),
       median_abs_diff = median(abs_delta),
       mean_pct_delta = mean(pct_delta))
}

print.small_data_table <- function(x, ...) {
  print(x, topn = 2)
}

dist_sinusoidal <- function(lat1, lon1, lat2, lon2, lambda0 = NA_real_) {
  .Call("Cdist_sinusoidal", lat1, lon1, lat2, lon2, lambda0, PACKAGE = "whichWithin")
}


