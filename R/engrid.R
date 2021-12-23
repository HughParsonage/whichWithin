#' Engrid
#' 
#' @param x,y \code{double(N)} Coordinates.
#' @param r Radius of each cell in the grid.
#' 
#' @export
engrid <- function(x, y, r) {
  .Call("Cengrid", x, y, r, PACKAGE = "whichWithin")
}



