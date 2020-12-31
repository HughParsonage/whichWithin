#' Two dimensional cross join
#' 
#' @param u,v Two vectors.
#' @param as_data_table (bool) Return a \code{data.table} (as opposed to a list, the default)?
#' @param new_cols The name of the list/\code{data.table}.
#' @param nThread The number of thread to use.
#' 
#' @return
#' A list (or \code{data.table} if requested), representing the 
#' expanded grid as if the data cross joined.
#' 
#' @export

cj2 <- function(u, v,
                as_data_table = TRUE,
                new_cols = c("id1", "id2", "u1", "v1", "u2", "v2"),
                nThread = 1L) {
  stopifnot(length(u) == length(v), 
            is.numeric(u), is.numeric(v),
            length(new_cols) == 6)
  ans <- Z4P(u, v, nThread)
  if (as_data_table) {
    setDT(ans)
    setnames(ans, new_cols)
    return(ans[])
  } else {
    return(setNames(ans, new_cols))
  }
}

