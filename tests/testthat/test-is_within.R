test_that("test for timing", {
  expect_true(TRUE)
})

test_that("is_within works for chains", {
  skip_if_not_installed("data.table")
  lat <- c(0, 0, 0, 1, 1, 1)
  lon <- c(1, 2, 2.5, 2.5, 3.5, 4)
  DT_chains <- data.table(lat, lon)
  actual <- which_within_cj(lat, lon, radius = 111, use_geosphere = FALSE)
  answer <- which_within(lat, lon, radius = 111)
  expect_equal(actual, answer)
  
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
  for (test_r in c(0.5, 1)) {
    res <- do_is_within(melb_latlons10k$LATITUDE,
                        melb_latlons10k$LONGITUDE,
                        r = test_r)
    
    actual <-
      cj2(melb_latlons10k$LATITUDE, 
          melb_latlons10k$LONGITUDE,
          as_data_table = TRUE,
          new_cols = paste0("V", 1:6)) 
    actual[, d := hutils::haversine_distance(V3, V4, V5, V6)]
    
    all_indices <- 
      actual[d < test_r, union(V1, V2)]
    
    all_indices <- all_indices[order(all_indices)]
    
    
    # The indices of is within should be the indices where
    # the distance is less than 1.
    if (identical(which(res), all_indices)) {
      expect_identical(which(res), all_indices)
    } else {
      # Maximum distance should be 10 m
      wres <- which(res)
      wres <- wres[wres %notin% all_indices]
      
      
      mind <- actual[`|`(V1 %in% wres, V2 %in% wres), min(d)]
      expect_gt(mind, test_r - 0.01)
    }
    
    
    
    res_are_within <- are_within(melb_latlons10k$LATITUDE, 
                                 melb_latlons10k$LONGITUDE,
                                 r = paste0(test_r, " km"))
    if (identical(which(res_are_within), all_indices)) {
      expect_identical(which(res_are_within), all_indices)
    } else {
      # Maximum distance should be 10 m
      wres <- which(res_are_within)
      wres <- wres[wres %notin% all_indices]
      
      if (length(wres)) {
        mind <- actual[`|`(V1 %in% wres, V2 %in% wres), min(d)]
      } else {
        # avoid the warning -- still correct
        mind <- Inf
      }
      expect_gt(mind, test_r - 0.01)
      
      wres <- which(res_are_within)
      wres <- all_indices[all_indices %notin% wres]
      
      if (length(wres)) {
        mind <- actual[`|`(V1 %in% wres, V2 %in% wres), min(d)]
      } else {
        # avoid the warning -- still correct
        mind <- Inf
      }
      expect_gt(mind, test_r - 0.01)
    }
  }
  
})


test_that("is_within_pixels coverage", {
  # Want to test coinciding counts
  lat <- rep(seq(-36, -35.6, length.out = 101), each = 3)
  lon <- rep(seq(140, 140.1, length.out = 101), each = 3)
  expect_true(all(is_within_pixels(lat, lon, 2, 140)))
  
  
})


test_that("are_within error handling", {
  expect_error(are_within(0, 1:2), regexp = "length")
  if (is_64bit()) {
    expect_error(are_within(d231 <- double(2^31), d231), 
                 regexp = "long|allocate")
  }
  expect_error(are_within(c(-35, -36), c(150, 151)), regexp = "sorted")
  expect_error(are_within(c(-35, -36), c(152, 151)), regexp = "sorted")
})

test_that("mutate_are_within", {
  skip_if_not_installed("fst")
  melb_latlons.fst <- system.file("extdata", "melb-latlons.fst", package = "whichWithin")
  skip_if_not(file.exists(melb_latlons.fst))
  melb_latlons <- fst::read_fst(melb_latlons.fst, as.data.table = TRUE, from = 1, to = 101)
  orig_ADDRESS_DETAIL_INTRNL_ID <- copy(melb_latlons[["ADDRESS_DETAIL_INTRNL_ID"]])
  melb_latlons_df <- as.data.frame(melb_latlons)
  melb_latlons_col <-
    mutate_are_within(melb_latlons, radius = "50km")
  expect_identical(melb_latlons$ADDRESS_DETAIL_INTRNL_ID, orig_ADDRESS_DETAIL_INTRNL_ID)
  
  
  melb_latlons_key <-
    setkey(copy(melb_latlons), LATITUDE, LONGITUDE) %>%
    .[, "are_within_50km" := are_within(LATITUDE, LONGITUDE, radius = "50km")] %>%
    setkey(ADDRESS_DETAIL_INTRNL_ID)
  
  expect_identical(melb_latlons_key$are_within_50km,
                   melb_latlons_col$are_within_50km)
  
  
})

test_that("NA handling", {
  x <- c(1, 2, 3, NA, 4)
  expect_equal(first_non_na_dbl(x), 1)
  y <- c(NA, x)
  expect_equal(first_non_na_dbl(y), 1)
  z <- c(NA, NA_real_)
  expect_equal(first_non_na_dbl(z, fill = 5), 5)
  z0 <- double(0)
  expect_equal(first_non_na_dbl(z0, fill = 5), 5)
  
  base_lat <- c(seq(-36, -35.5, length.out = 67), NA)
  base_lon <- c(seq(145, 145.5, length.out = 67), NA)
  
  DT <- CJ(lat = base_lat,
           lon = base_lon)
  # Sort it
  base_lat <- DT$lat
  base_lon <- DT$lon
  
  expect_equal(are_within(base_lat, base_lon, radius = 3),
               are_within_for(base_lat, base_lon, radius = 3))
  
})




