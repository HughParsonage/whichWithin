
is_64bit <- function() {
  .Machine$sizeof.pointer == 8L
}
