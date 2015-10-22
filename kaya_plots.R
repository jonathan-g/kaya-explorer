library(readxl)
library(magrittr)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(grid)


kaya_plot <- function(kaya, country, v, y_lab) {
  p <- eval(substitute(
    ggplot(kaya %>% filter(country == q2 & ! is.na(q1)), aes(x = year, y = q1)),
    list(q1 = as.name(v), q2 = country)))
  p + geom_point(size=3, color = I("dark blue")) +
    geom_line(size=1, color = I("dark blue")) +
    labs(x = "Year", y = y_lab) +
    theme_bw(base_size = 20) +
    theme(axis.title.y = element_text(vjust=1.2), axis.title.x = element_text(vjust=-0.1))
}
