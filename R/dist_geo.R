
dist_geo <- function(lat1, lon1, lat2, lon2) {
  p1 <- cbind(lon1, lat1)
  p2 <- cbind(lon2, lat2)
  
  geosphere::distGeo(p1, p2) / 1000
}


