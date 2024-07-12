.onAttach <- function(...) {
  ver <- utils::packageVersion("kayaExplorer")
  msg <- stringr::str_c("\nThis is kayaExplorer version ", as.character(ver), "\n")
  packageStartupMessage(msg)
}

.onLoad <- function(libname, pkgname) {
  op <- options()
  op.kayaExplorer <- list(
    kayaExplorer.rstudio = FALSE
  )
  set_ops <- !(names(op.kayaExplorer) %in% names(op))
  if (any(set_ops)) options(op.kayaExplorer[set_ops])

  setup_globals()

  invisible()
}
