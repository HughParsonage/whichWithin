test_that("do_is_within2 works", {
  skip_if_not_installed("fst")
  skip_if_not_installed("withr")
  melb_latlons.fst <- system.file("extdata", "melb-latlons.fst", package = "whichWithin")
  skip_if_not(file.exists(melb_latlons.fst))
  melb_latlons <- fst::read_fst(melb_latlons.fst, as.data.table = TRUE)
  withr::with_seed(1, {
    melb_latlons10k <- melb_latlons[hutils::samp(1:.N, size = fifelse(is_64bit(), 10e3, 1e3))]
  })
  setkey(melb_latlons10k, LATITUDE, LONGITUDE)
  res <- do_is_within2(melb_latlons10k$LATITUDE,
                       melb_latlons10k$LONGITUDE,
                       r = 1, 
                       145)
  actual <-
    cj2(melb_latlons10k$LATITUDE, 
        melb_latlons10k$LONGITUDE,
        as_data_table = TRUE,
        new_cols = paste0("V", 1:6)) 
  actual[, d := hutils::haversine_distance(V3, V4, V5, V6)]
  
  all_indices <- 
    actual[d < 1, union(V1, V2)]
  
  all_indices <- all_indices[order(all_indices)]
  expect_equal(sum(res), length(all_indices), scale = 1, tol = 110)
  
  
  
  
  
  
})
