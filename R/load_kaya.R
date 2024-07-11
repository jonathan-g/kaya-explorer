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

export_kaya <- function(kaya, region) {
  x <- region
  data <- kaya %>% dplyr::filter(.data$region == x) %>%
    dplyr::select(Year = "year", "P", "G", "E", "F") %>%
#    dplyr::mutate(P = .data$P * 1E+6, G = .data$G * 1E+6, F = .data$F * 44 / 12)
    dplyr::mutate(P = .data$P * 1E+6, G = .data$G * 1E+6)
  filename <- stringr::str_c(gsub(" +", "_", region), '.csv', sep = " ")
  readr::write_csv(data, file = filename)
}

{
  assign("kaya_regions", kayadata::kaya_region_list(),
         envir = .globalvars)
  assign("td_regions", unique(kayadata::td_trends$region) %>%
           purrr::keep(~.x %in% get("kaya_regions", envir = .globalvars)),
         envir = .globalvars)
  assign("ebf_regions", unique(kayadata::fuel_mix$region) %>%
           purrr::keep(~.x %in% get("kaya_regions", envir = .globalvars)),
         envir = .globalvars)
}
