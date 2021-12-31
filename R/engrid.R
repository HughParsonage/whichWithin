#' Engrid
#' @description Create a grid of radius \code{r} over the range of \code{x, y},
#' index them and return the gridpoint of each point.
#' 
#' 
#' @param x,y \code{double(N)} Coordinates.
#' @param r Radius of each cell in the grid.
#' 
#' @export
engrid <- function(x, y, r) {
  .Call("Cengrid", x, y, r, PACKAGE = "whichWithin")
}


merge2 <- function(e, ew = as.integer(sqrt(max(e))), time, time_radius = 3600L) {
  .Call("Cmerge2int", e, ew, time, time_radius, PACKAGE = "whichWithin")
}



