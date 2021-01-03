#' Are points within a set distance?
#' @param lat,lon Coordinates of points.
#' @param radius Radius in kiometres.
#' @param lambda0 Passed to the sinusoidal projection. Defaults to the 
#' median of \code{lon}. Use to indicate the longitude at which distortion
#' should be zero (and smaller the the closer the longitude). 
#' 
#' @return Logical vector \code{x} such that,
#' for every \code{(lat[i],lon[i])}, \code{x[i]} is
#' \code{TRUE} iff there is some \code{j} such that 
#' \code{dist_geo(lat[i], lon[i], lat[j], lon[j]) < r}.
#' 
#' For \code{mutate_are_within}, returns the data frame \code{DT}
#' with a logical column \code{new_col}, whether the point is within
#' \code{radius} of other points in the data frame.
#' 
#' @export

are_within <- function(lat, lon, radius = "1 km", lambda0 = NULL) {
  r <- units2km(radius)
  if (length(lat) != length(lon)) {
    stop("`length(lat) = ", length(lat), "`, but ", 
         "`length(lon) = ", length(lon), ". ", 
         "Lengths of `lat` and `lon` must be equal.")
  }
  if (length(lat) > .Machine$integer.max) {
    stop("`lat` is a long vector: `length(lat) = ", prettyNum(length(lat)), "`, ",
         "which is not supported.")
  }
  
  latlonsorted <- is_sorted2(lat, lon, c(NA, NA))
  not_sorted <- anyNA(latlonsorted) || !all(latlonsorted, na.rm = TRUE)
  if (not_sorted) {
    stop("lat,lon not sorted.")
  }
  if (is.null(lambda0)) {
    lambda0 <- lon[length(lat) %/% 2]
  }
  
  is_within_pixels(lat, lon, r, lambda0)
}

#' @rdname are_within
#' @param DT A data.frame, \code{data.table} preferred. Ideally, already
#' \code{key(DT)} are the \code{lat,lon} columns in \code{DT}, as this will
#' improve performance by obviating the need to sort internally.
#' @param new_col (\code{character(1)}) The name of the new column to set on \code{DT}.
#' If \code{NULL}, the default, the column name will be \code{are_within_<radius>}.
#' @param overwrite (\code{logical(1)}) Should \code{new_col} be overwritten
#' if \code{DT} already has a column so named? Set to \code{FALSE}
#' to error, \code{NA} for a warning, and \code{TRUE} to overwrite silently.
#' 
#' @param latloncols \code{character(2)}. The columns in \code{DT} 
#' corresponding to latitude and longitude respectively. If \code{"auto"}, 
#' the default, columns will be guessed.
#' 
#' 
#' @export
mutate_are_within <- function(DT, 
                              radius = "1 km", 
                              lambda0 = NULL, 
                              new_col = NULL,
                              overwrite = NA,
                              latloncols = "auto") {
  stopifnot(is.data.frame(DT))
  was_data_table <- is.data.table(DT)
  if (!was_data_table) {
    orig_attributes <- attributes(DT)
    setDT(DT)
  }
  new_col_uses_km <- !is.character(radius) || endsWith(radius, "km")
  
  r <- units2km(radius)
  if (is.null(new_col)) {
    new_col <- paste0("are_within_", 
                      r,
                      fifelse(new_col_uses_km, "km", "m"))
  }
  if (hasName(DT, new_col) && !isTRUE(overwrite)) {
    if (isFALSE(overwrite)) {
      stop(vname(DT), " contained column '", new_col, "' so aborting as requested.")
    } else {
      warning(vname(DT), " already contains column '", new_col, "' so overwriting. Set ",
              "`overwrite = FALSE` to silence this message.")
    }
  }
  
  checkmate::assert_character(latloncols)
  
  if (identical(latloncols, "auto")) {
    latloncols <- plausible_latloncols(DT)
  }
  
  latlon_already_sorted <- 
    OR(AND(length(key(DT)) >= 2,
           identical(key(DT)[1:2], latloncols)),
       all(is_sorted2(.subset2(DT, latloncols[1]),
                      .subset2(DT, latloncols[2]),
                      c(NA, NA))))
  
  # 
  if (!latlon_already_sorted) {
    # If not already sorted, add a column 'i' to keep track
    # of the original ordering, sort by lat,lon,
    # apply are_within() then restore the original order
    
    DTi <- DT[, .SD, .SDcols = c(latloncols)]
    DTi[, "i" := seq_len(.N)]
    lat <- lon <- NULL
    setnames(DTi, latloncols, c("lat", "lon"))
    setkeyv(DTi, c("lat", "lon"))
    DTi[, (new_col) := are_within(lat, lon, radius = r, lambda0 = lambda0)]
    setorderv(DTi, "i")
    # Now in original order
    ans <- .subset2(DTi, new_col)
    DTi <- NULL
  } else {
    lat <- .subset2(DTi, latloncols[1])
    lon <- .subset2(DTi, latloncols[2])
    ans <- are_within(lat, lon, radius = r, lambda0 = lambda0)
  }
  
  
  # Don't plonk 'ans' in case it's already a name
  DT[, (new_col) := FALSE]
  set(DT, j = new_col, value = ans)
  if (was_data_table) {
    return(DT[])
  }
  
  # restore e.g. tibbles, data.frames
  attributes(DT) <- orig_attributes
  DT
}

