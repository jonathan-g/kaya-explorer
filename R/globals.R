#' @include kt_startup.R
NULL

.globalvars <- new_environment(parent = emptyenv())

.shinyenv <- new_environment(parent = emptyenv())

setup_globals <- function() {
  assign("debugging", FALSE, envir = .globalvars)

  purrr::iwalk(c(mass.c = 12, mass.co2 = 44),
               ~assign(.y, .x, envir = .globalvars))

  purrr::iwalk(c(answer.bg = '#FFFFA0', answer.fg = '#A000A0'),
               ~assign(.y, .x, envir = .globalvars))


  # purrr::iwalk(
  #   list(
  #     normal.body.props = cellProperties(padding.left = 2, padding.right = 2),
  #     normal.head.props = cellProperties(padding.top = 2,
  #                                        padding.left = 2, padding.right = 2),
  #     answer.body.props = cellProperties(padding.left = 2, padding.right = 2,
  #                                        background.color = answer.bg),
  #     answer.head.props = cellProperties(padding.top = 2,
  #                                        padding.left = 2, padding.right = 2,
  #                                        background.color = answer.bg)
  #   ),
  #   ~assign(.y, .x, envir = .globalvars)
  # )

  purrr::iwalk(
    list(
      normal.body.props = list(
        pr_t = officer::fp_text(font.size = 12),
        pr_p = officer::fp_par(padding.left = 2,
                               padding.right = 2,
                               padding.top = 2),
        pr_c = officer::fp_cell()
      ),
      normal.head.props = list(
        pr_t = officer::fp_text(font.size = 12, bold = TRUE),
        pr_p = officer::fp_par(padding.left = 2,
                               padding.right = 2,
                               padding.top = 2),
        pr_c = officer::fp_cell()
      ),
      answer.body.props = list(
        pr_t = officer::fp_text(font.size = 12),
        pr_p = officer::fp_par(padding.left = 2,
                               padding.right = 2,
                               padding.top = 2),
        pr_c = officer::fp_cell(background.color =
                                  get("answer.bg", envir = .globalvars))
      ),
      answer.head.props = list(
        pr_t = officer::fp_text(font.size = 12, bold = TRUE),
        pr_p = officer::fp_par(padding.left = 2,
                               padding.right = 2,
                               padding.top = 2),
        pr_c = officer::fp_cell(background.color =
                                  get("answer.bg", envir = .globalvars))
      )
    ),
    ~assign(.y, .x, envir = .globalvars)
  )

  assign("kaya_labels",
         dplyr::tibble(
           variable = c('P','G','E','F','g','e', 'f', 'ef', 'gef'),
           varname = c('P', 'G', 'E', 'F', 'g', 'e', 'f', 'ef', 'CO2 per capita'),
           unit = c('billion', 'trillion dollars', 'quad','MMT CO2',
                    '$1000 per person', 'quad per $trillion', 'MMT per quad',
                    'metric ton per $ million', 'metric ton'),
           long.unit = c('billion people', 'trillion dollars', 'quad',
                         'million metric tons CO2', '$1000 per person',
                         'quad of energy per $trillion  GDP',
                         'million metric tons CO2 per quad',
                         'metric ton CO2 per million dollars GDP',
                         'metric ton CO2'),
           long.long = c('Population', 'GDP', 'Energy consumption', 'Emissions',
                         'Per-capita GDP', 'Energy intensity of the economy',
                         'CO2 intensity of the energy supply',
                         'CO2 intensity of the economy', "Per-capita emissions"),
           long = c('Population', 'GDP', 'Energy consumption', 'Emissions',
                    'Per-capita GDP', 'Energy intensity',
                    'CO2 intensity of energy', 'CO2 intensity of economy',
                    "Per-capita emissions"),
           short = c('P', 'GDP', 'Energy', 'Emissions',
                     'Per-capita GDP', 'Energy intensity',
                     'Carbon intensity', 'Carbon intensity of economy',
                     'Per-capita emissions')
         ),
         envir = .globalvars
  )

  # quads per MTOE
  assign("mtoe", 1 / 25.2, envir = .globalvars)

}

#' Set debugging flag
#'
#' Sets debugging flag to output debugging info to the console.
#'
#' @param dbg Logical debugging flag (`TRUE` or `FALSE`).
#'
#' @return The previous value of the debugging flag.
#'
#' @examples
#' set_debug(TRUE)
#'
#' @export
#'
set_debug <- function(dbg = NULL) {
  old_dbg <- get("debugging", envir = .globalvars)
  if (is.null(dbg) || is.na(dbg)) {
    dbg <- ! isTRUE(old_dbg)
  }
  assign("debugging", dbg, envir = .globalvars)
  old_dbg
}
