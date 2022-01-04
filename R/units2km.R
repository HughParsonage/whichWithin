#' Convert a distance as a string to a number 
#' @description Standardizes input like \code{"500 m"}, \code{"0.5 km"} and 
#' \code{0.5} (numeric) to a single double, (in this case, \code{0.5}). Useful
#' so the user can clearly express the units, rather than entering a bare 
#' numeric.
#' 
#' @param input The input to standardize.
#' 
#' @return If \code{input} is numeric, then it is returned, immediately.
#' 
#' Otherwise, if a string, the distance in kilometres
#' the string appears to represent. 
#' 
#' @examples
#' units2km(5) 
#' units2km("5 km")
#' units2km("50 m")
#' 
#' @export

units2km <- function(input) {
  if (is.numeric(input)) {
    return(input)
  }
 
  # hutilscpp:::units2km
  stopifnot(is.character(input),
            length(input) == 1L)
  string <- input
  # put km before m!
  if (endsWith(string, "km")) {
    dist_km <- sub("\\s*km$", "", string)
    # use as.double here and as.numeric later to separate warning msgs
    dist_km <- as.double(dist_km)
  } else if (endsWith(string, "m")) {
    dist_km <- sub("\\s*m$", "", string)
    dist_km <- as.numeric(dist_km) / 1000
  }
  stopifnot(!anyNA(dist_km), is.numeric(dist_km))
  dist_km
}
