#' Find matches by distance and time
#' @param x An integer vector, an index of \code{Data}, to restrict the values
#' of \code{orig} in the result.
#' @param Data A \code{data.table} containing columns for latitude and longitude
#' (see \code{\link{plausible_latloncols}}), as well as columns \code{time_column}
#' and \code{x_column}.
#' @param x_column,time_column Names/indices of \code{Data} corresponding to integer
#' vectors for filtering and determining time-matching. 
#' 
#' @param distance A string, passed to \code{\link{units2km}}: the maximum distance between matches.
#' @param duration The maximum difference in time in seconds.
#' 
#' @param option An integer. Only \code{option = 0L} is allowed currently.
#' \describe{
#' \item{0}{Restricts values of \code{orig}; any exact matches mean approximate matches are ignored.}
#' \item{1}{Same as \code{0} but \code{dest} is restricted too.}
#' \item{2}{Exact matches do not preclude approximate matches}
#' }
#' 
#' @param Ion Used to set the expected number of rows required in the output. Memory is allocated
#' in large chunks within this function. If you find yourself running out of memory
#' (with an error message referring to a \code{realloc}) you can tweak this parameter.
#' A slight overestimate is much better than a slight underestimate as an insufficient
#' initial allocation will cause a large reallocation when required. (e.g. if 
#' 100M rows are required and \code{Ion = 99e6} then approximately 160M rows will 
#' be reallocated).
#' 
#' 
#' @return A list of two integer vectors, named \code{orig} and \code{dest}, 
#' the indices of \code{Data} corresponding to pairs of observations that 
#' are within \code{distance} and \code{duration} of each other. Will be a \code{data.table}
#' if \code{length(orig) <= INT_MAX}.
#' 
#' @export
which_within_dist_dura <- function(x, 
                                   Data,
                                   x_column,
                                   time_column, 
                                   distance = "100m", 
                                   duration = 3600,
                                   option = 0L,
                                   Ion = getOption("whichWithin.Ion", 10)) {
  stopifnot(identical(option, 0L))  # tempo
  
  stopifnot(is.data.table(Data), 
            length(time_column) == 1L,
            hasName(Data, time_column),
            is.integer(.subset2(Data, time_column)),
            length(x_column) == 1L,
            hasName(Data, x_column),
            is.integer(.subset2(Data, x_column)))
  
  if (!isntSorted(x, asc = TRUE)) {
    x <- sort(x)
  }
  maybe_latloncols <- plausible_latloncols(Data, "Data")
  Keyz <- key(Data)
  if (is.null(Keyz)) {
    setkeyv(Data, maybe_latloncols)
  }
  stopifnot(is.character(distance))  # Because we don't want to misinterpret numbers as m ? km
  distance <- units2km(distance) * 1000
  
  Data_x <- .subset2(Data, x_column)
  lat <- .subset2(Data, "lat")
  lon <- .subset2(Data, "lon")
  Data_time <- .subset2(Data, time_column)
  stopifnot(is.double(lat), is.double(lon))
  stopifnot(is.double(Ion), length(Ion) == 1, !anyNA(Ion), 
            Ion >= 0.5)
  
  ans <- .Call("Capprox_dvr_matches",
               x,
               as.double(distance),
               as.double(duration),
               Data_x,
               lat, lon,
               Data_time, 
               option,
               Ion,
               PACKAGE = "whichWithin")
  
  names(ans) <- c("orig", "dest")
  if (max(lengths(ans)) <= .Machine$integer.max) {
    setDT(ans)
  }
  ans
}

od2dd <- function(lat, lon, time, orig, dest, option = 0L) {
  .Call("C_od2dd", lat, lon, time, orig, dest, option, PACKAGE = "whichWithin")
}


