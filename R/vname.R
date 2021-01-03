


vname <- function(x, use_checkmate = FALSE) {
  if (use_checkmate) {
    checkmate::vname(x)
  } else {
    # input 'a' -> 'a' not '\'a\''
    as.character(eval.parent(substitute(substitute(x))))
  }
}