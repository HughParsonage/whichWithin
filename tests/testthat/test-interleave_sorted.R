test_that("interleave_sorted works", {
  x <- c(0L, 1L, 1L, 2L, 5L, 15L)
  y <- c(-1L, 1L, 1L, 2L, 4L, 14L)
  expect_identical(interleave_sorted(x, y), sort(c(x, y), na.last = FALSE))
})
