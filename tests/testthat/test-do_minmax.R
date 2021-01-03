test_that("do_minmax works", {
  x <- runif(100)
  y <- runif(100)
  ans <- do_minmax(x, y)
  actual <- c(min(x), max(x), min(y), max(y))
  expect_equal(ans, actual)
})
