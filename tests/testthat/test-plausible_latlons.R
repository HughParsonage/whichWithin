test_that("plausible_latloncols identifies obvious names", {
  Llatlon <- list(lat = 1, lon = 2, phat = 0, phong = 2)
  expect_equal(plausible_latloncols(Llatlon), c("lat", "lon"))
  
  LLatitudeLongitude <- list(A = 1, 
                             B = 2,
                             LatitudeOK = TRUE,
                             lon_not_provided = 0,
                             Latitude = -35,
                             Longitude = 150)
  expect_equal(plausible_latloncols(LLatitudeLongitude), c("Latitude", "Longitude"))
  
  LNoLat <- list(lon = 1)
  expect_error(plausible_latloncols(LNoLat), "No names for latitude")
  LNoLon <- list(Latitude = 1, Y = 2)
  expect_error(plausible_latloncols(LNoLon), "No names for longitude")
  
})
