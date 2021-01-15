#' Which points are within a radius of each other?
#' @description Used to approiximately but relatively quickly
#' identify among a collection of coordinates which are within
#' a certain radius. 
#' 
#' 
#' @param lat,lon Numeric vectors of equal length defining the latitude
#' and longitude of the points. Must be sorted by latitude then by longitude.
#' 
#' Long vectors not supported.
#' 
#' @param radius Either a single number designating the desired radius
#' in kilometres or a string specifying the same.
#' @param latlonsorted A logical vector of length 1 or 2 indicating 
#' whether it is known whether \code{lat,lon} are known to already be sorted.
#' If \code{NA}, the default, then the vectors are checked, with an error
#' if unsorted.
#' @param lambda0 Passed to the sinusoidal projection. Defaults to the 
#' median of \code{lon}. Use to indicate the longitude at which distortion
#' should be zero (and smaller the the closer the longitude). 
#' 
#' @param incl_dist (\code{TRUE | FALSE}) If \code{TRUE},
#' an extra column is added that gives the distance between the relevant points.
#' 
#' @param id (\code{integer(N)}) An optional vector specifying an alternative 
#' index as opposed to using  
#' the index of \code{lat,lon} in the result. Only used if an integer vector
#' of the same length as \code{lat}.
#' 
#' @return 
#' A \code{data.table} of two integer columns, \code{orig} and \code{dest}, 
#' the indices of \code{lat,lon}
#' 
#' 
#' @examples
#' library(data.table)
#' DT <- data.table(lat = runif(1000, -35, -34),
#'                  lon = runif(1000, 150, 151))
#' setkey(DT, lat, lon)
#' DT[, which_within(lat, lon, lambda0 = 150.5)]
#' DT[, which_within(lat, lon, radius = "150 m")]
#' DT[, which_within(lat, lon, incl_dist = TRUE)]
#' 
#' 
#' 
#' 
#' @export
which_within <- function(lat, lon,
                         radius = "1 km", 
                         latlonsorted = NA,
                         lambda0 = mean(lon, na.rm = TRUE),
                         id = NULL,
                         incl_dist = FALSE) {
  r <- units2km(radius)
  if (length(lat) != length(lon)) {
    stop("`length(lat) = ", length(lat), "`, but ", 
         "`length(lon) = ", length(lon), ". ", 
         "Lengths of `lat` and `lon` must be equal.")
  }
  if (length(lat) > .Machine$integer.max) {
    stop("`lat` is a long vector: `length(lat) = ", 
         prettyNum(length(lat), big.mark = ","), "`, ",
         "which is not supported.")
  }
  
  latlonsorted <- is_sorted2(lat, lon, latlonsorted)
  not_sorted <- anyNA(latlonsorted) || !all(latlonsorted, na.rm = TRUE)
  if (not_sorted) {
    stop("lat,lon not sorted.")
  }
  
  out <- do_which_within(lat, lon, r = r, lambda0 = lambda0,
                         incl_distance = isTRUE(incl_dist))
  if (!isTRUE(incl_dist)) {
    out <- out[1:2]
  }
  setDT(out)
  setattr(out, "sorted", c("orig", "dest"))
  if (!is.null(id)) {
    orig <- .subset2(out, "orig")
    dest <- .subset2(out, "dest")
    set(out, j = "orig", value = id[orig])
    set(out, j = "dest", value = id[dest])
  }
  
  out[]
}


which_within_cj <- function(lat, lon, radius = "1 km",
                            use_geosphere = TRUE, 
                            keep_DT = FALSE) {
  stopifnot(is.numeric(lat), 
            is.numeric(lon),
            length(lat) == length(lon))
  r <- units2km(radius)
  
  d <- orig <- dest <- lat1 <- lon1 <- lat2 <- lon2 <- NULL
  
  CJ12 <- cj2(lat, lon, new_cols = c("orig", "dest", "lat1", "lon1", "lat2", "lon2"))
  
  if (isTRUE(use_geosphere) && requireNamespace("geosphere", quietly = TRUE)) {
    CJ12[, d := dist_geo(lat1, lon1, lat2, lon2)]
  } else {
    CJ12[, d := haversine_distance(lat1, lon1, lat2, lon2)]
  }
  setkeyv(CJ12, c("orig", "dest"))
  if (keep_DT) {
    return(CJ12[])
  }
  
  CJ12[d < r, .SD, .SDcols = c("orig", "dest")]
}



