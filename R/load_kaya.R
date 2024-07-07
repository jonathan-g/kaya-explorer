#' @include globals.R


mtoe = 1 / 25.2 # quads

data("kaya_data", package = "kayadata")


translate_regions <- function(df) {
  invisible(df)
}

filter_values <- function(df) {
  bad_regions <- df %>% dplyr::group_by(region) %>%
    dplyr::summarize_at(vars(P, G, E, F), ~sum(!is.na(.))) %>%
    dplyr::ungroup() %>% dplyr::mutate(n = pmin(P, G, E, F)) %>% dplyr::filter(n < 5)
  df %>% dplyr::anti_join(bad_regions, by = "region")
}

filter_trends <- function(df) {
  df %>% dplyr::filter_at(vars(P, G, E, F, g, e, f), dplyr::all_vars(!is.na(.)))
}

load_kaya <- function() {
  this_env = environment()
  data("kaya_data", package = "kayadata", envir = this_env)
  kaya_data %>% translate_regions() %>%
    filter_values() %>%
    dplyr::mutate(gef = F / (1000 * P)) %>%
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
  data <- kaya %>% dplyr::filter(region == x) %>%
    dplyr::select(Year = year, P,G,E,F) %>%
#    dplyr::mutate(P = P * 1E+6, G = G * 1E+6, F = F * 44 / 12)
    dplyr::mutate(P = P * 1E+6, G = G * 1E+6)
  filename <- paste(gsub(" +", "_", region), '.csv')
  write_csv(data, file = filename)
}

load_energy_by_fuel <- function() {
  this_env = environment()
  data("fuel_mix", package="kayadata", envir = this_env)
  fuel_mix %>% translate_regions() %>%
    invisible()
}
