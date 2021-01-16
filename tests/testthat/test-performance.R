test_that("sufficiently fast", {
  skip_on_cran()
  skip_on_ci()
  skip_if_not_installed("bench")
  skip_if_not_installed("fst")
  skip_if_not_installed("withr")
  melb_latlons.fst <- system.file("extdata", "melb-latlons.fst", package = "whichWithin")
  skip_if_not(file.exists(melb_latlons.fst))
  melb_latlons <- fst::read_fst(melb_latlons.fst, as.data.table = TRUE)
  withr::with_seed(1, {
    melb_latlons100k <- melb_latlons[hutils::samp(1:.N, size = 100e3)]
  })
  setkey(melb_latlons100k, LATITUDE, LONGITUDE)
  timing_for_100k_1km <- bench::system_time(do_is_within2(melb_latlons100k$LATITUDE,
                                                         melb_latlons100k$LONGITUDE,
                                                         r = 1, 
                                                         145))
  
  # Time should be less than one second
  expect_lt(as.double(timing_for_100k_1km)[2], 1)
  
})
