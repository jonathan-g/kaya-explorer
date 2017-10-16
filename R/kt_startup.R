.onAttach <- function(...) {
  ver <- utils::packageVersion("kayatool")
  msg <- paste0("\nThis is kayatool version ", ver, "\n")
  packageStartupMessage(msg)
}

.onLoad <- function(libname, pkgname) {
  op <- options()
  op.kaya_tool <- list(
    kaya_tool.rstudio = FALSE
  )
  set_ops <- !(names(op.kaya_tool) %in% names(op))
  if (any(set_ops)) options(op.kaya_tool[set_ops])
  invisible()
}