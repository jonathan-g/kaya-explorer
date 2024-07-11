.onAttach <- function(...) {
  ver <- utils::packageVersion("kayatool")
  msg <- stringr::str_c("\nThis is kayatool version ", as.character(ver), "\n")
  packageStartupMessage(msg)
}

.onLoad <- function(libname, pkgname) {
  op <- options()
  op.kayatool <- list(
    kayatool.rstudio = FALSE
  )
  set_ops <- !(names(op.kayatool) %in% names(op))
  if (any(set_ops)) options(op.kayatool[set_ops])

  setup_globals()

  invisible()
}
