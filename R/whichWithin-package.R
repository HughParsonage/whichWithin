#' whichWithin
#' 
#' @import data.table
#' @importFrom checkmate vname
#' @importFrom hutils %notin%
#' @importFrom hutils AND
#' @importFrom hutils OR
#' @importFrom hutils haversine_distance
#' @importFrom Rcpp evalCpp
#' @importFrom magrittr and
#' @importFrom magrittr or
#' @importFrom stats complete.cases
#' @importFrom stats median
#' @importFrom stats setNames
#' @importFrom utils hasName

#' @keywords internal
"_PACKAGE"

# The following block is used by usethis to automatically manage
# roxygen namespace tags. Modify with care!
## usethis namespace: start
#' @useDynLib whichWithin, .registration = TRUE
## usethis namespace: end
NULL
