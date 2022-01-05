#' Convert a string into a duration in seconds
#' @param x A string, like "2 minutes" representing a duration
#' @return An integer, that duration in seconds.
#' @export
#' 

string2duration <- function(x) {
  stopifnot(length(x) == 1, is.character(x),
            !is.na(x), grepl("[0-9]", x))
  
  if (endsWith(x, "seconds") ||
      endsWith(x, "secs") ||
      endsWith(x, "sec") ||
      grepl("[0-9]\\s*s$", x)) {
    return(as.integer(gsub("[^0-9]", "", x)))
  }
  v <- as.double(gsub("[^0-9.]", "", x))
  m <- 1
  if (endsWith(x, "minutes") ||
      endsWith(x, "mins") ||
      endsWith(x, "min")) {
    return(as.integer(60 * v))
  }
  
  if (endsWith(x, "hours") ||
      endsWith(x, "hrs") ||
      endsWith(x, "hr")) {
    return(as.integer(3600 * v))
  }
  if (requireNamespace("lubridate", quietly = TRUE)) {
    as.double(lubridate::duration(x))
  }
  stop("Duration could not be determined.") # nocov
  
}
