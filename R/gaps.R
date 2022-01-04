#' Gaps
#' @description For an integer vector (such as time) find gaps, and 
#' return a index based "sufficiently continguous" groups.
#' 
#' @param x An integer vector.
#' @param r The minimum radius such that \eqn{|x_i - x_j| > r} implies
#' \eqn{x_i,x_j} are not in the same group. Will be coerced to as.integer(ceil(r)).
#' 
#' @return For each element \code{x} a group index.
#' @export

group_contiguous <- function(x, r) {
  stopifnot(is.integer(x), 
            is.numeric(r), length(r) == 1, !is.na(r), r >= 0)
  if (is.double(r)) {
    r <- as.integer(ceil(r))
  }
  rx <- NULL
  if (isntSorted(x, asc = TRUE)) {
    rx <- frank(x, ties.method = "first")  # for reentry
    ox <- order(x)
    x <- x[ox]
  }
  ans <- .Call("Cgroup_contiguous", x, r, PACKAGE = "whichWithin")
  if (!is.null(rx)) {
    return(ans[rx])
  }
  ans
}
