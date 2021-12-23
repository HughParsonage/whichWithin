first_non_na_dbl <- function(x, fill = 0) {
  .Call("Cfirst_non_na_dbl", x, fill, PACKAGE = "whichWithin")
}