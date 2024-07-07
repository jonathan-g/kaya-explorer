#' @include kt_startup.R
NULL

.globalvars <- new_environment(parent = emptyenv())

.shinyenv <- new_environment(parent = emptyenv())

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
