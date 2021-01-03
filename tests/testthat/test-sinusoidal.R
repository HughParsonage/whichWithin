test_that("Sinusoidal works", {
  skip_if_not_installed("mapproj")
  skip_if_not_installed("data.table")
  DT <- data.table(lat = seq(-35, -30, length.out = 101),
                   lon = seq(149, 152, length.out = 101))
  
  DT[, c("x_mpp", "y_mpp") := mapproj::mapproject(lon, lat, projection = "sinusoidal")[1:2]]
  DT[, c("x_sin", "y_sin") := Sinusoidal(lat, lon, mean(lon))]
  
  
  expect_equal(DT$x_mpp, DT$x_sin)
  expect_equal(DT$y_mpp, DT$y_sin)
  
  
})
