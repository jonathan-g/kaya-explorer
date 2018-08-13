library(magrittr)
library(tidyverse)
library(readxl)
library(stringr)
library(janitor)


mtoe = 1 / 25.2 # quads

data("kaya_data", package = "kayadata")


translate_countries <- function(df) {
  invisible(df)
}

filter_values <- function(df) {
  bad_countries <- df %>% group_by(country) %>%
    summarize_at(vars(P, G, E, F), funs(sum(!is.na(.)))) %>%
    ungroup() %>% mutate(n = pmin(P, G, E, F)) %>% filter(n < 5)
  df %>% anti_join(bad_countries, by = "country")
}

filter_trends <- function(df) {
  df %>% filter_at(vars(P, G, E, F, g, e, f), all_vars(!is.na(.)))
}

load_kaya <- function() {
  this_env = environment()
  data("kaya_data", package = "kayadata", envir = this_env)
  kaya_data %>% translate_countries() %>%
    filter_values() %>%
    invisible()
}

load_top_down <- function() {
  this_env = environment()
  data("td_trends", package="kayadata", envir = this_env)
  td_trends %>% translate_countries() %>%
    filter_trends() %>%
    invisible()
}

export_kaya <- function(kaya, country) {
  x <- country
  data <- kaya %>% filter(country == x) %>%
    select(Year = year, P,G,E,F) %>%
    mutate(P = P * 1E+6, G = G * 1E+6, F = F * 44 / 12)
  filename <- paste(gsub(" +", "_", country), '.csv')
  write_csv(data, file = filename)
}

load_energy_by_fuel <- function() {
  this_env = environment()
  data("fuel_mix", package="kayadata", envir = this_env)
  fuel_mix %>% translate_countries() %>%
    invisible()
}
