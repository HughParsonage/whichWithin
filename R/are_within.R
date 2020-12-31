#' Are points within a set distance?
#' @param lat,lon Coordinates of points.
#' @param radius Radius in kiometres.
#' @param lambda0 Passed to the sinusoidal projection. Defaults to the 
#' median of \code{lon}. Use to indicate the longitude at which distortion
#' should be zero (and smaller the the closer the longitude). 
#' 
#' @return Logical vector \code{x} such that,
#' for every \code{(lat[i],lon[i])}, \code{x[i]} is
#' \code{TRUE} iff there is some \code{j} such that 
#' \code{dist_geo(lat[i], lon[i], lat[j], lon[j]) < r}.
#' 
#' @export

are_within <- function(lat, lon, radius = "1 km", lambda0 = NULL) {
  r <- units2km(radius)
  if (length(lat) != length(lon)) {
    stop("`length(lat) = ", length(lat), "`, but ", 
         "`length(lon) = ", length(lon), ". ", 
         "Lengths of `lat` and `lon` must be equal.")
  }
  if (length(lat) > .Machine$integer.max) {
    stop("`lat` is a long vector: `length(lat) = ", prettyNum(length(lat)), "`, ",
         "which is not supported.")
  }
  
  latlonsorted <- is_sorted2(lat, lon, latlonsorted)
  not_sorted <- anyNA(latlonsorted) || !all(latlonsorted, na.rm = TRUE)
  if (not_sorted) {
    stop("lat,lon not sorted.")
  }
  if (is.null(lambda0)) {
    lambda0 <- lon[length(lat) %/% 2]
  }
  
  is_within_pixels(lat, lon, r, lambda0)
}

