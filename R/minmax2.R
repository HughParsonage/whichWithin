

minmax2 <- function(x, y) {
  .Call("Cdo_minmax", x, y, PACKAGE = "whichWithin")
}

