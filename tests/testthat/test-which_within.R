test_that("which_within works", {
  skip_if_not_installed("geosphere")
  skip_if_not_installed("data.table")
  
  for (quadratic in c(FALSE, TRUE)) {
    for (radius in c(0.5, 1, 5, 10, 20)) {
      if (quadratic) {
        base_lat <- -36 + 2 * seq(0, 1, length.out = 37)^2
        base_lon <- 145 + seq(0, 1, length.out = 37)^2
      } else {
        base_lat <- seq(-36, -34, length.out = 37)
        base_lon <- seq(145, 146, length.out = 37)
      }
      DT <- CJ(lat = base_lat,
               lon = base_lon)
      
      expected <- which_within_cj(DT$lat, DT$lon, radius = radius)
      
      ans <- which_within(DT$lat, DT$lon, radius = radius)
      ans_upr <- which_within(DT$lat, DT$lon, radius = radius * 1.005)
      ans_lwr <- which_within(DT$lat, DT$lon, radius = radius * 0.995)
      
      if (!identical(ans, expected)) {
        expect_true(nrow(ans_upr) >= nrow(expected),
                   info = paste("quadratic", quadratic, " ",
                                "radius", radius, "(>=)"))
        expect_true(nrow(ans_lwr) <= nrow(expected),
                   info = paste("quadratic", quadratic, " ",
                                "radius", radius, "(<=)"))
      } else {
        expect_identical(ans, expected, 
                         info = paste("quadratic", quadratic, " ",
                                      "radius", radius))
      }
    }
  }
})

test_that("Copes with NA", {
  skip_on_cran()
  skip_if_not(is_64bit())
  base_lat <- c(seq(-36, -35.75, length.out = 67), NA)
  base_lon <- c(seq(145, 145.25, length.out = 67), NA)
  DT <- CJ(lat = base_lat,
           lon = base_lon)
  
  ans <- which_within(DT$lat, DT$lon, radius = "750m")
  exp <- which_within_cj(DT$lat, DT$lon, radius = "750m", use_geosphere = FALSE)
  expect_identical(ans, exp)
  
})

test_that("id column", {
  DT <- data.table(id = c(4L, 5L, 1L, -5L), 
                   lat = c(0, 0.05, 0.1, 1),
                   lon = c(0, 0.05, 0, 1))
  expect_equal(which_within(DT$lat, DT$lon, radius = 10, id = DT$id),
               data.table(orig = c(4L, 5L),
                          dest = c(5L, 1L)))
  
  
})

test_that("which_within2", {
  library(data.table)
  library(hutilscpp)
  DT <- data.table(lat = -runif(101, 37, 37.1), 
                   lon = runif(101, 144, 146),
                   VisitDateTime = as.integer(runif(101, 10, 401)),
                   CaseNumber = 1:101,
                   key = "lat,lon")
  DT <- data.table(lat = seq(-35, -34, length.out = 101),
                   lon = 150,
                   VisitDateTime = as.integer(runif(101, 10, 401)),
                   CaseNumber = 1:101,
                   key = "lat,lon")
  DT[, i := .I]
  DT[, j := .I]
  Cj <- DT[DT, on = .(i > i), nomatch = 0L]
  Cj[, dist := hutils::haversine_distance(lat, lon, i.lat, i.lon)]
  Cj[, dist_y := abs(lat - i.lat)]
  Cj[, dist_x := abs(lon - i.lon)]
  Cj[, dura := abs(VisitDateTime - i.VisitDateTime)]
  setkey(Cj, lat, lon)
  
  n <- Cj[, sum_and3s(dist_y <= (0.000009007702 * 10000), dist_x <= 0.000011352150 * 10000, dura <= 1)]
  
  wj <- whichWithin:::approx_dvr_matches(1:100, distance = 10000, duration = 1,
                                         Data = DT)
  expect_equal(n, nrow(wj))
})



