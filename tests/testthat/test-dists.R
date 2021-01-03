test_that("haversine_dist works", {
  haversine_dists <- 
    sapply(seq(-37, -35, length.out = 101), 
           haversine_dist, 150, -37, 150.2)
  
  haversine_distances <- 
    sapply(seq(-37, -35, length.out = 101), 
           hutils::haversine_distance, 150, -37, 150.2)
  
  expect_equal(haversine_dists, haversine_distances)
})
