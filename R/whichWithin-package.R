#' whichWithin
#' 
#' @import data.table
#' @importFrom checkmate vname
#' @importFrom fastmatch fmatch
#' @importFrom hutils %notin%
#' @importFrom hutils AND
#' @importFrom hutils OR
#' @importFrom hutils haversine_distance
#' @importFrom hutilscpp and3s
#' @importFrom hutilscpp isntSorted
#' @importFrom magrittr %>%
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
