

test_that("which_within_x1d", {
  library(data.table)
  DT <- data.table(x = c(rep(1L, 5L), rep(2L, 5L)),
                   y = rep(1:5, 2))
  ans <- which_within_x1d(DT$x, DT$y, r = 1L)
  expect_equal(ans, .which_within_x1d_naive(DT$x, DT$y, r = 1L))
})

