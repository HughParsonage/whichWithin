test_that("engrid works", {
  expect_equal(engrid(c(0, 0.1, 2), c(0, 0.1, 2), r = 0.25), 
               c(0L, 0L, 63L))
})
