#' Which names are plausibly latlons
#' @param DT An object with names.
#' @param vname_DT The name of the table to return in error messages.
#' 
#' @return A character vector of length-2, the names of \code{DT} that
#' will be plausibly be lat,lon.
#' 
#' @export

plausible_latloncols <- function(DT, vname_DT = "DT") {
  noms <- names(DT)
  alat <- agrep("latitude", noms, ignore.case = TRUE)
  alon <- agrep("longitude", noms, ignore.case = TRUE)
  lat_nom <- lon_nom <- NULL
  if (length(alat) == 1) {
    lat_nom <- noms[alat]
  }
  if (length(alon) == 1) {
    lon_nom <- noms[alon]
  }
  if (!is.null(lat_nom) && !is.null(lon_nom)) {
    return(c(lat_nom, lon_nom))
  }
  
  if (length(alat) == 0) {
    warning("No obvious names for latitude in `", vname_DT, "`.")
    alat <- agrep("lat", noms, ignore.case = TRUE)
  }
  if (length(alon) == 0) {
    warning("No obvious names for longitude in `", vname_DT, "`.")
    alon <- agrep("lon", noms, ignore.case = TRUE)
  }
  if (length(alat) == 0) {
    stop("No names for latitude.")
  } else if (length(alat) > 1) {
    warning("Multiple plausible names for latitude. Did you mean ", toString(noms[alat]))
  }
  if (length(alon) == 0) {
    stop("No names for longitude.")
  } else if (length(alat) > 1) {
    warning("Multiple plausible names for longitude. Did you mean ", toString(noms[alon]))
  }
  noms[c(alat[1], alon[1])]
}

