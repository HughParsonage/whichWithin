test_that("which_within works", {
  skip_if_not_installed("geosphere")
  skip_if_not_installed("data.table")
  library(data.table)
  
  for (quadratic in c(FALSE, TRUE)) {
    for (radius in c(0.5, 1, 5, 10, 20)) {
      if (quadratic) {
        base_lat <- -36 + 2 * seq(0, 1, length.out = 37)^2
        base_lon <- 145 + seq(0, 1, length.out = 37)^2
      } else {
        base_lat <- seq(-34, -36, length.out = 37)
        base_lon <- seq(-34, 145, length.out = 37)
      }
      DT <- CJ(lat = base_lat,
               lon = base_lon)
      
      expected <- which_within_cj(DT$lat, DT$lon, radius = radius)
      
      ans <- which_within(DT$lat, DT$lon, radius = radius)
      expect_identical(ans, expected, 
                       info = paste("quadratic", quadratic, " ",
                                    "radius", radius))
    }
  }
  
  
  
  
  
})
