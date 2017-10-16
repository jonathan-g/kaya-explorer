library(magrittr)
library(tidyverse)
# library(grid)
library(lazyeval)


kaya_plot <- function(kaya, countries, v, y_lab = NULL) {
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

  countries = as.list(countries)

  .dots = interp(~(country %in% countries & !is.na(v)), v = as.name(v))
  data <- kaya %>% filter_(.dots = .dots)

  if (length(countries) > 1) {
    color_scale <- scale_color_brewer(palette = "Dark2")
    legend = guides(color = guide_legend(title = "Country"),
                    shape = guide_legend(title = "Country"))
  } else {
    color_scale = scale_color_manual(values = "dark blue")
    legend = guides(color = FALSE, shape = FALSE)
  }

  p <- ggplot(data, aes_string(x = "year", y = v, shape = "country", color = "country"))
  p + geom_point(size = 3) + geom_line(size = 1) +
    color_scale +
    legend +
    labs(x = "Year", y = y_lab) +
    theme_bw(base_size = 20) +
    theme(axis.title.y = element_text(vjust=1.2),
          axis.title.x = element_text(vjust=-0.1),
          legend.key = element_rect(color = NA))
}
