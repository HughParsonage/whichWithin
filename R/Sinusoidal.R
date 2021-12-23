Sinusoidal <- function(lat, lon, lambda0 = NA_real_) {
  .Call("CSinusoidal", lat, lon, lambda0, PACKAGE = "whichWithin")
}