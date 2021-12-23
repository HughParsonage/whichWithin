
haversine_dist <- function(olat1, olon1, olat2, olon2) {
  stopifnot(is1d(olat1), is1d(olon1), 
            is1d(olat2), is1d(olon2))
  .Call("Chaversine_dist", olat1, olon1, olat2, olon2, PACKAGE = "whichWithin")
}

is1d <- function(x) is.double(x) && length(x) == 1

