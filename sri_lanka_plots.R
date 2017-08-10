library(pacman)
p_load(tidyverse)
p_load(stringr)

source('kaya_tool/load_kaya.R', chdir = T)

kaya_list <- load_kaya(data.dir = 'kaya_tool/data')
kaya <- kaya_list$kaya
sri_lanka <- kaya %>% filter(country == 'Sri Lanka')

p_load(tidyverse)
p_load(readxl)
p_load(magrittr)
p_load(stringr)
p_load(grid)
p_load(lazyeval)

kaya_plot <- function(kaya, v, y_lab = NULL, scale = NULL) {
  labels <- c(P =  'Population (millions)',
              G =  'Gross Domestic Product ($ billion)',
              E =  'Energy consumption (quads)',
              F =  'Carbon emissions (MMT)',
              g =  'Per-capita GDP (thousand dollars)',
              e =  'Energy intensity of economy (quad per $trillion)',
              f =  'Carbon intensity of energy (MMT per quad)',
              ef = 'Carbon intensity of economy (MMT per $trillion)'
  )

  scales <- c(P =  1.0E+3,
              G =  1.0E+3,
              E =  1,
              F =  1,
              g =  1,
              e =  1,
              f =  1,
              ef = 1
  )

  if (is.null(y_lab)) y_lab <- labels[v]
  if (is.null(scale)) scale <- scales[v]

  .dots = interp(~(!is.na(v)), v = as.name(v))
  data <- kaya %>% filter_(.dots = .dots)

  p <- ggplot(data, aes_string(x = "year", y = paste0(v, " * scale"))) +
    geom_point(size = 1.5, color = "dark blue") +
    geom_line(size = 1, color = "dark blue") +
    labs(x = "Year", y = y_lab) +
    theme_bw(base_size = 10) +
    theme(axis.title.y = element_text(vjust=1.2),
          axis.title.x = element_text(vjust=-0.1),
          legend.key = element_rect(color = NA))
}


e_vs_gdp <- ggplot(filter(kaya, geography == "country", year == 2010),
                   aes(x = E/P, y = G/P)) +
  geom_point(color = "dark blue", alpha = 0.5) +
  scale_x_log10(name = "Per-capita energy consumption (billion BTU)") +
  scale_y_log10(name = "Per-capita GDP (thousand dollars)") +
  theme_bw(base_size = 10)

ggsave(filename = "energy_vs_gdp.png", plot = e_vs_gdp,
       width = 1024 / 300, height = 1024 / 300)

pop <- kaya_plot(sri_lanka, "P")
ggsave(filename = "population.png", plot = pop,
       width = 1024 / 300, height = 1024 / 300)

gdp <- kaya_plot(sri_lanka, "G")
ggsave(filename = "gdp.png", plot = gdp,
       width = 1024 / 300, height = 1024 / 300)

energy <- kaya_plot(sri_lanka, "E")
ggsave(filename = "energy.png", plot = energy,
       width = 1024 / 300, height = 1024 / 300)

emissions <- kaya_plot(sri_lanka, "F")
ggsave(filename = "emissions.png", plot = emissions,
       width = 1024 / 300, height = 1024 / 300)

gdp_pc <- kaya_plot(sri_lanka, "g")
ggsave(filename = "gdp_pc.png", plot = gdp_pc,
       width = 1024 / 300, height = 1024 / 300)

energy_pc <- kaya_plot(sri_lanka, "e")
ggsave(filename = "energy_pc.png", plot = energy_pc,
       width = 1024 / 300, height = 1024 / 300)

emissions_pc <- kaya_plot(sri_lanka, "f")
ggsave(filename = "emissions_pc.png", plot = emissions_pc,
       width = 1024 / 300, height = 1024 / 300)

carbon_intensity <- kaya_plot(sri_lanka, "ef")
ggsave(filename = "carbon_intensity.png", plot = carbon_intensity,
       width = 1024 / 300, height = 1024 / 300)

