test_that("cj2 works", {
  x <- runif(2); y <- runif(2)
  out <- cj2(x, y)
  expect_equal(nrow(out), 1)
  
  x3 <- runif(3); y3 <- runif(3)
  out <- cj2(x3, y3, as_data_table = FALSE, new_cols = paste0("V", 1:6))
  expect_equal(length(out[[1]]), 3)
  expect_equal(names(out), paste0("V", 1:6))
  expect_true(is.list(out))
})
