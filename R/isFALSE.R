
isFALSE <- function(x) is.logical(x) && length(x) == 1L && !anyNA(x) && !x
