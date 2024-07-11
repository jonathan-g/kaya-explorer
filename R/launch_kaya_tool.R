# kayatool is open-source software; you can redistribute it and/or
# modify it under the terms of the MIT License as published by the Open Source
# Initiative.
#
# kayatool is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
# FITNESS FOR A PARTICULAR PURPOSE.  See the MIT License for more details.
#
# You should have received a copy of the MIT License along with this program; if
# not, see <https://opensource.org/licenses/MIT>.

#' @include globals.R ui.R server.R
NULL



#' Get a shinyApp object for the Kaya tool app
#'
#' Generate a shinyApp object for the Kaya tool app.
#'
#' @return A shinyApp object that can be run to launch the app.
#'
#' @export
#'
kaya_app <- function() {
  app <- shiny::shinyApp(
    ui = get("ui", envir = .shinyenv),
    server = get("server", envir = .shinyenv),
    options = list(launch.browser = launch.browser, ...)
  )
}

#' Launch the kayatool app
#'
#' Launch the kayatool app in the default web browser. RStudio
#' users also have the option of launching the app in RStudio's pop-up Viewer.
#'
#' @param rstudio Only relevant for RStudio users. The default (\code{FALSE}) is
#'   to launch the app in the user's default web browser rather than RStudio's
#'   pop-up Viewer. Users can change the default to \code{TRUE} by setting the
#'   global option \code{options(kayatool.rstudio = TRUE)}.
#' @param ... Optional arguments passed to \code{\link[shiny]{runApp}}.
#'
#' @return Nothing is returned
#'
#' @examples
#' \dontrun{
#' launch_kaya_tool()
#' }
#'
#' @export
#'
launch_kaya_tool <- function(rstudio = getOption("kayatool.rstudio"), ...) {
  message("\nLaunching kayatool interface.")
  invisible(launch(rstudio, ...))
}

# Internal launch function
# @param rstudio launch in rstudio viewer instead of web browser?
# @param ... passed to shiny::runApp
launch <- function(rstudio = FALSE, ...) {
  launch.browser <- if (!rstudio)
    TRUE else getOption("shiny.launch.browser", interactive())

  app <- kaya_app()

  shiny::runApp(
    appDir = app
    )
}
