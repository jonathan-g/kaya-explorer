library(magrittr)
library(dplyr)
library(stringr)
library(janitor)


mtoe = 1 / 25.2 # quads

data("kaya_data", package = "kayadata")


translate_regions <- function(df) {
  invisible(df)
}

filter_values <- function(df) {
  bad_regions <- df %>% group_by(region) %>%
    summarize_at(vars(P, G, E, F), funs(sum(!is.na(.)))) %>%
    ungroup() %>% mutate(n = pmin(P, G, E, F)) %>% filter(n < 5)
  df %>% anti_join(bad_regions, by = "region")
}

filter_trends <- function(df) {
  df %>% filter_at(vars(P, G, E, F, g, e, f), all_vars(!is.na(.)))
}

load_kaya <- function() {
  this_env = environment()
  data("kaya_data", package = "kayadata", envir = this_env)
  kaya_data %>% translate_regions() %>%
    filter_values() %>%
    invisible()
}

load_top_down <- function() {
  this_env = environment()
  data("td_trends", package="kayadata", envir = this_env)
  td_trends %>% translate_regions() %>%
    filter_trends() %>%
    invisible()
}

export_kaya <- function(kaya, region) {
  x <- region
  data <- kaya %>% filter(region == x) %>%
    select(Year = year, P,G,E,F) %>%
#    mutate(P = P * 1E+6, G = G * 1E+6, F = F * 44 / 12)
    mutate(P = P * 1E+6, G = G * 1E+6)
  filename <- paste(gsub(" +", "_", region), '.csv')
  write_csv(data, file = filename)
}

load_energy_by_fuel <- function() {
  this_env = environment()
  data("fuel_mix", package="kayadata", envir = this_env)
  fuel_mix %>% translate_regions() %>%
    invisible()
}
