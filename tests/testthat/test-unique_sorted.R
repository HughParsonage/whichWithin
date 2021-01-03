test_that("unique_sorted works", {
  x <- c(0L, 1L, 1L, 2L, 5L, 15L)
  expect_identical(unique_sorted(x), unique(x))
})

test_that("unique_sorted_inplace works", {
  x <- c(0L, 1L, 1L, 2L, 5L, 15L)
  ans <- unique(x)
  expect_identical(unique_sorted_inplace(x), ans)
  expect_identical(x[seq_along(ans)], ans)
})


