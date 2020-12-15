
which_within <- function(lat, lon, radius = "1 km") {
  r <- units2km(radius)
  do_which_within(lat, lon, r)
}


which_within_cj <- function(lat, lon, radius = "1 km", use_geosphere = TRUE) {
  stopifnot(is.numeric(lat), 
            is.numeric(lon),
            length(lat) == length(lon))
  r <- units2km(radius)
  DT1 <- copy(DT2 <- data.table(i2 = seq_along(lat), lat, lon))
  DT1[, i1 := .I]
  DT1[, i_1 := .I]
  DT2[, i_2 := .I]
  CJ12 <- DT1[DT2, on = "i1<i2", allow.cartesian = TRUE, nomatch = 0L]
  if (isTRUE(use_geosphere) && requireNamespace("gesophere", quietly = TRUE)) {
    CJ12[, d := dist_geo(lat, lon, i.lat, i.lon)]
  } else {
    CJ12[, d := haversine_distance(lat, lon, i.lat, i.lon)]
  }
  CJ12[d < r, .(orig = i_1, dest = i_2)]
}



