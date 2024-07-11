#' @include globals.R

kaya_plot <- function(kaya, regions, v, y_lab = NULL) {
  debugging <- get("debugging", envir = .globalvars)
  message("Kaya Plot: kaya is a ", stringr::str_c(class(kaya), collapse = ", "),
          " and regions = ", stringr::str_c(as.list(regions), collapse = ", "),
          " and variable = ", v)
  labels <- c(P =  'Population (billions)',
              G =  'Gross Domestic Product ($ trillion)',
              E =  'Energy consumption (quads)',
              F =  'Fossil-fuel carbon emissions (million metric tons)',
              g =  'Per-capita GDP ($ thousand)',
              e =  'Energy intensity of economy (quads per $trillion)',
              f =  'Carbon intensity of energy supply (MMT per quad)',
              ef = 'Carbon intensity of economy (MMT per $trillion)'
  )

  if (is.null(y_lab)) y_lab <- labels[v]

  regions = as.list(regions)

  data <- kaya %>% dplyr::filter(.data$region %in% regions, !is.na(!!sym(v)))

  if (length(regions) > 1) {
    color_scale <- scale_color_brewer(palette = "Dark2")
    legend = guides(color = guide_legend(title = "Country/Region"),
                    shape = guide_legend(title = "Country/Region"))
  } else {
    color_scale = scale_color_manual(values = "dark blue")
    legend = guides(color = FALSE, shape = FALSE)
  }

  if (debugging) message("In kaya_plot, data is a ",
                         stringr::str_c(class(data), collapse = ", "))
  p <- ggplot(data, aes(x = .data$year, y = .data[[v]],
                        shape = .data$region,
                        color = .data$region))
  p + geom_point(size = 3) + geom_line(linewidth = 1) +
    color_scale +
    legend +
    labs(x = "Year", y = y_lab) +
    theme_bw(base_size = 20) +
    theme(axis.title.y = element_text(vjust=1.2),
          axis.title.x = element_text(vjust=-0.1),
          legend.key = element_rect(color = NA))
}
