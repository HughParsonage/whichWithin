#' Which exact and within
#' @description Used to identify pairs that have exact matches in one 
#' vector and near matches in another.
#' 
#' @param x A vector to identify exact matches. 
#' @param y A vector to identify near matches.
#' @param r The radius within which matches of \code{y} occur.
#' @param option For backwards compatible changes.
#' 
#' @return
#' Considering \code{x, y} as coordinates, we define the 
#' binary relation
#' \deqn{(x[i], y[i]) \mathrm{within} (x[j], y[j])}
#' if \eqn{x[i] = y[j]} and \eqn{|y[i] - y[j]| < r}).
#' 
#' 
#' A list of two elements, \code{orig} and \code{dest} giving the 
#' indices of \code{x, y} that satisfy the binary relation above.
#' @export


which_within_x1d <- function(x, y, r, option = 1L) {
  if (!is.integer(x) || !is.integer(y)) {
    if (!is.integer(x)) {
      ux <- unique(x)
      x <- fmatch(x, ux)
    }
    if (!is.integer(y)) {
      uy <- unique(y)
      y <- fmatch(y, uy)
    }
    oxy <- order(x, y)
    x <- x[oxy]
    y <- y[oxy]
  }
  stopifnot(is.integer(r))
  
  ans <- .Call("Cwhich_within_x1d", x, y, r, option, PACKAGE = "whichWithin")
  names(ans) <- c("orig", "dest")
  if (max(lengths(ans)) <= .Machine$integer.max) {
    setDT(ans)
  }
  ans
}

.which_within_x1d_naive <- function(x, y, r) {
  stopifnot(is.integer(x), is.integer(y), is.integer(r))
  CJ1 <- CJ(i1 = seq_along(x), i2 = seq_along(y))[i1 < i2]
  CJ1[, x1 := x[i1]]
  CJ1[, y1 := y[i1]]
  CJ1[, x2 := x[i2]]
  CJ1[, y2 := y[i2]]
  CJ1[x1 == x2][abs(y2 - y1) <= r, .(orig = i1, dest = i2)]
}


