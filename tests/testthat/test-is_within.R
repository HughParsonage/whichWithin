test_that("is_within works for chains", {
  skip_if_not_installed("data.table")
  library(data.table)
  lat <- c(0, 0, 0, 1, 1, 1)
  lon <- c(1, 2, 2.5, 2.5, 3.5, 4)
  DT_chains <- data.table(lat, lon)
  actual <- which_within_cj(lat, lon)
  
})


test_that("is_within works", {
  skip_if_not_installed("fst")
  melb_latlons.fst <- system.file("extdata", "melb-latlons.fst", package = "whichWithin")
  skip_if_not(file.exists(melb_latlons.fst))
  melb_latlons <- fst::read_fst(melb_latlons.fst, as.data.table = TRUE)
  withr::with_seed(1, {
    melb_latlons10k <- melb_latlons[hutils::samp(1:.N, size = fifelse(is_64bit(), 10e3, 1e3))]
  })
  setkey(melb_latlons10k, LATITUDE, LONGITUDE)
  res <- do_is_within(melb_latlons10k$LATITUDE,
                      melb_latlons10k$LONGITUDE,
                      r = 1)
  
  actual <-
    cj2(melb_latlons10k$LATITUDE, 
        melb_latlons10k$LONGITUDE,
        as_data_table = TRUE,
        new_cols = paste0("V", 1:6)) 
  actual[, d := hutils::haversine_distance(V3, V4, V5, V6)]
  
  all_indices <- 
    actual[d < 1, union(V1, V2)]
  
  all_indices <- all_indices[order(all_indices)]
  
  
  # The indices of is within should be the indices where
  # the distance is less than 1.
  expect_identical(which(res), all_indices)
  
})
