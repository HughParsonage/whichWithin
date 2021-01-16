

avbl_ram_gb <- function() {
  if (requireNamespace("memuse", quietly = TRUE)) {
    Sys_memuse <- memuse::Sys.meminfo()[["freeram"]]
    prefixes_IEC <- c("B", "KiB", "MiB", "GiB", "TiB", "PiB", "EiB", "ZiB", "YiB")
    prefixes__SI <- c("B", "KB", "MB", "GB", "TB", "PB", "EB", "ZB", "YB")
    unit <- Sys_memuse@unit
    m <- match(unit, prefixes_IEC, nomatch = 0L)
    if (!m) {
      m <- match(unit, prefixes__SI, nomatch = 0L)  
    }
    size_gb <- Sys_memuse@size * (10 ^ (m - match("GB", prefixes__SI, nomatch = 0L)))
    return(size_gb)
  }
}
