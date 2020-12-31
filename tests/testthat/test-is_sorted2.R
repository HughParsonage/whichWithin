test_that("is_sorted2 works", {
  skip_if_not_installed("data.table")
  skip_if_not_installed("datasets")
  library(datasets)
  library(data.table)
  DT <- data.table(x = mtcars$mpg, y = mtcars$disp)
  # ignores values if set
  expect_identical(is_sorted2(DT$x, DT$y, TRUE), TRUE)
  expect_identical(is_sorted2(DT$x, DT$y, FALSE), FALSE)
  expect_identical(is_sorted2(DT$x, DT$y, c(TRUE, TRUE)), c(TRUE, TRUE))
  expect_identical(is_sorted2(DT$x, DT$y, c(TRUE, FALSE)), c(TRUE, FALSE))
  
  expect_identical(is_sorted2(DT$x, DT$y, NA), FALSE)
  expect_identical(is_sorted2(DT$x, DT$y, c(NA, NA)), c(FALSE, FALSE))
  setkey(DT, x)
  expect_identical(is_sorted2(DT$x, DT$y, c(NA, NA)), c(TRUE, FALSE))
  setkey(DT, x, y)
  expect_identical(is_sorted2(DT$x, DT$y, c(NA, NA)), c(TRUE, TRUE))
  
})
