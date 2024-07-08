#' @include globals.R


translate_regions <- function(df) {
  invisible(df)
}

filter_values <- function(df) {
  bad_regions <- df %>%
    dplyr::summarize(dplyr::across(c("P", "G", "E", "F"), ~sum(!is.na(.x))),
                     .by = "region") %>%
    dplyr::mutate(n = pmin(.data$P, .data$G, .data$E, .data$F)) %>%
    dplyr::filter(.data$n < 5)
  df %>% dplyr::anti_join(bad_regions, by = "region") %>%
    invisible()
}

filter_trends <- function(df) {
  df %>% dplyr::filter(! dplyr::if_any(c("P", "G", "E", "F", "g", "e", "f"),
                                       is.na)) %>%
    invisible()
}

append_gef <- function(df) {
  df %>% dplyr::mutate(gef = .data$F / (1000 * .data$P)) %>%
    invisible()
}

append_gef_trends <- function(df) {
  df %>% dplyr::mutate(gef = .data$F - .data$P) %>%
    invisible()
}

extract_kaya <- function(region) {
  kayadata::get_kaya_data(region, quiet = TRUE) %>%
    filter_values() %>%
    append_gef() %>%
    invisible()
}

extract_td_trends <- function(region) {
  kayadata::get_top_down_trends(region, quiet = TRUE) %>%
    filter_trends() %>%
    append_gef_trends() %>%
    invisible()
}

extract_fuel_mix <- function(region, collapse_renewables = FALSE) {
  kayadata::get_fuel_mix(region, collapse_renewables = collapse_renewables) %>%
    invisible()
}

load_kaya <- function() {
  get("kaya_data", envir = .globalvars) %>%
    translate_regions() %>%
    filter_values() %>%
    append_gef() %>%
    invisible()
}

load_top_down <- function() {
  get("td_trends", envir = .globalvars) %>%
    translate_regions() %>%
    filter_trends() %>%
    invisible()
}

export_kaya <- function(kaya, region) {
  x <- region
  data <- kaya %>% dplyr::filter(.data$region == x) %>%
    dplyr::select(Year = "year", "P", "G", "E", "F") %>%
#    dplyr::mutate(P = .data$P * 1E+6, G = .data$G * 1E+6, F = .data$F * 44 / 12)
    dplyr::mutate(P = .data$P * 1E+6, G = .data$G * 1E+6)
  filename <- paste(gsub(" +", "_", region), '.csv')
  readr::write_csv(data, file = filename)
}

load_energy_by_fuel <- function() {
  get("fuel_mix", envir = .globalvars) %>%
    translate_regions() %>%
    invisible()
}

# kaya <- load_kaya() %>% dplyr::mutate(F = c_to_co2(F), f = c_to_co2(f), ef = c_to_co2(ef))
assign("kaya", load_kaya() %>% dplyr::mutate(id = seq(dplyr::n())),
       envir = .globalvars)
# message("Kaya data loaded")

assign("top_down", load_top_down(), envir = .globalvars)
# message("Top-down data loaded")


assign("energy_by_fuel", load_energy_by_fuel(), envir = .globalvars)
# message("Fuel mixes loaded")

{
  assign("kaya_regions", unique(get("kaya", envir = .globalvars)$region),
         envir = .globalvars)
  assign("td_regions",
         as.character(unique(get("top_down", envir = .globalvars)$region)) %>%
           purrr::keep(~.x %in% get("kaya_regions", envir = .globalvars)),
         envir = .globalvars)
  assign("ebf_regions",
         unique(get("energy_by_fuel", envir = .globalvars)$region) %>%
           purrr::keep(~.x %in% get("kaya_regions", envir = .globalvars)),
         envir = .globalvars)
}
