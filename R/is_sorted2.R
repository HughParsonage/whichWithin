
is_sorted2 <- function(x, y, s2) {
  .Call("Cis_sorted2", x, y, s2, PACKAGE = "whichWithin")
}


