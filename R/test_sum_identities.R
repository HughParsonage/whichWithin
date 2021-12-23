
test_sum_identities <- function(k, j, N) {
  .Call("Ctest_sum_identities", k, j, N, PACKAGE = "whichWithin")
}

