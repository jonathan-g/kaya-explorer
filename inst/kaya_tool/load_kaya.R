library(magrittr)
library(tidyverse)
library(readxl)
library(stringr)
library(janitor)


mtoe = 1 / 25.2 # quads

prepare_kaya <- function() {
  world_bank_translations = c(", +(Islamic|Arab) +Rep\\. *$" = "",
                              "^Korea, +Rep\\. *$" = "South Korea",
                              " +SAR, +China *$" = "",
                              " +members *$" = "", ", +RB *$" = "")

  bp_translations = c("^Total +" = "", "&" = "and", "of which: +" = "",
                      "^US$" = "United States", "Slovakia" = "Slovak Republic",
                      "China Hong Kong SAR" = "Hong Kong")

  # Data from World Bank
  # https://data.worldbank.org/
  # https://data.worldbank.org/indicator/SP.POP.TOTL
  pop_file = system.file(file.path("kaya_tool", "data", "API_SP.POP.TOTL_DS2_en_csv_v2.csv"),
                         package = "kayatool")
  # Data from World Bank
  # https://data.worldbank.org/indicator/NY.GDP.MKTP.KD
  # GDP constant 2010 US dollars
  # Because PPP adjusted GDP is not available before 1990
  gdp_file = system.file(file.path("kaya_tool", "data", "API_NY.GDP.MKTP.KD_DS2_en_csv_v2.csv"),
                         package = "kayatool")
  # Data from BP Statistical Review of World Energy
  # https://www.bp.com/en/global/corporate/energy-economics/statistical-review-of-world-energy/downloads.html
  energy_file = system.file(file.path("kaya_tool", "data", "bp-statistical-review-of-world-energy-2017-underpinning-data.xlsx"),
                            package = "kayatool")

  population = suppressWarnings(suppressMessages(read_csv(pop_file, skip = 4))) %>%
    clean_names() %>% select(-starts_with('indicator'), -x62) %>%
    gather(key = year, value = population, -(country_name:country_code)) %>%
    mutate(year = str_replace_all(year, '^x', '') %>% as.integer(),
           population = population * 1E-9)

  gdp = suppressWarnings(suppressMessages(read_csv(gdp_file, skip = 4))) %>%
    clean_names() %>% select(-starts_with('indicator'), -x62) %>%
    gather(key = year, value = gdp, -(country_name:country_code)) %>%
    mutate(year = str_replace_all(year, '^x', '') %>% as.integer(),
           gdp = gdp * 1E-12)


  kaya_data = full_join(population, gdp, by = c("country_name", "country_code", "year")) %>%
    rename(P = population, G = gdp, country = country_name) %>%
    mutate(country = str_replace_all(country, world_bank_translations),
           country_code = factor(country_code),
           g = G / P)

  energy = read_excel(energy_file, "Primary Energy Consumption", range = "A3:BA94",
                      na = c('', 'NA', "na", "N/A/", "n/a")) %>%
    clean_names() %>% rename(country = million_tonnes_oil_equivalent) %>%
    filter(! is.na(country)) %>%
    gather(key = year, value = primary_energy_mtoe, -country) %>%
    mutate(year = str_replace(year, "^x", "") %>% as.integer(),
           country = str_replace_all(country, bp_translations),
           primary_energy_quads = primary_energy_mtoe * mtoe)

  carbon <- read_excel(energy_file,"Carbon Dioxide Emissions", range = "A3:BA94",
                       na = c('', 'NA', "na", "N/A/", "n/a")) %>%
    clean_names() %>% rename(country = million_tonnes_carbon_dioxide) %>%
    filter(! is.na(country)) %>%
    gather(key = year, value = mmt_co2, -country) %>%
    mutate(year = str_replace(year, "^x", "") %>% as.integer(),
           country = str_replace_all(country, bp_translations),
           mmtc = mmt_co2 * 12 / (12 + 32))

  energy = inner_join(energy, carbon, by = c('year', 'country')) %>%
    select(country, year, E = primary_energy_quads, F = mmtc)

  #
  # P = population in billions
  # G = world GDP in trillion dollars
  # E = energy consumption in quads
  # F = CO2 emissions in million tons of carbon
  #
  # g = thousand dollars per person
  # e = quads per trillion dollars
  # f = million tons of carbon per quad
  #
  kaya_data = kaya_data %>% inner_join(energy, by = c("country", "year")) %>%
    mutate(country = factor(country)) %>%
    mutate(e = E / G, f = F / E, ef = e * f)

  exclude_countries <- c('Other Middle East', 'North America')

  world <- c('World')
  regions <- c('North America')

  kaya_data = kaya_data %>%
    filter(! country %in% exclude_countries) %>%
    mutate(geography = ifelse(country %in% world, 'world',
                              ifelse(country %in% regions, 'region',
                                     'country')) %>%
             ordered(levels = c('country', 'region', 'world'))) %>%
    arrange(desc(geography), country, year)

  invisible(kaya_data)
}

load_kaya <- function() {
  kaya_data_file <- "data/kaya_data.Rds"
  if (file.exists(kaya_data_file)) {
    kaya_data <- read_rds(kaya_data_file)
  } else {
    kaya_data <- prepare_kaya()
    write_rds(kaya_data, kaya_data_file, compress = "bz2")
  }
  invisible(kaya_data)
}

prepare_top_down <- function() {
  P <- read_csv('data/World_population_by_region.csv', skip = 4,
                na = c('NA','N/A','')) %>%
    rename(nation = X1, r.P = `Growth (2012-2040)`) %>%
    select(nation, r.P) %>%
    dplyr::filter(! is.na(nation)) %>%
    mutate(r.P = r.P %>% str_extract('^.*?(?=%$)') %>% as.numeric() / 100)

  g <- read_csv('data/world_GDP_PPP.csv', skip = 4,
                na = c('NA','N/A','')) %>%
    rename(nation = X1, r.g = `Growth (2012-2040)`) %>%
    select(nation, r.g) %>%
    dplyr::filter(! is.na(nation)) %>%
    mutate(r.g = r.g %>% str_extract('^.*?(?=%$)') %>% as.numeric() / 100)

  e <- read_csv('data/World_energy_intensity_by_region.csv', skip = 4,
                na = c('NA','N/A','')) %>%
    rename(nation = X1, r.e = `Growth (2012-2040)`) %>%
    select(nation, r.e) %>%
    dplyr::filter(! is.na(nation)) %>%
    mutate(r.e = r.e %>% str_extract('^.*?(?=%$)') %>% as.numeric() / 100)

  top.down <- P %>% full_join(g, by = 'nation') %>%
    full_join(e, by = 'nation')
  # top.down <- translate_nations(top.down)
  invisible(top.down)
}

load_top_down <- function() {
  top_down_file <- "data/top_down.Rds"
  if (file.exists(top_down_file)) {
    top_down <- read_rds(top_down_file)
  } else {
   top_down <- prepare_top_down()
   write_rds(top_down, top_down_file, compress = "bz2")
  }
  invisible(top_down)
}

export_kaya <- function(kaya, country) {
  x <- country
  data <- kaya %>% filter(country == x) %>%
    select(Year = year, P,G,E,F) %>%
    mutate(P = P * 1E+6, G = G * 1E+6, F = F * 44 / 12)
  filename <- paste(gsub(" +", "_", country), '.csv')
  write_csv(data, file = filename)
}


prepare_energy_by_fuel <- function() {
  bp_translations = c("^Total +" = "", "&" = "and", "of which: +" = "",
                      "^US$" = "United States", "Slovakia" = "Slovak Republic",
                      "China Hong Kong SAR" = "Hong Kong")

  # Data from BP Statistical Review of World Energy
  # https://www.bp.com/en/global/corporate/energy-economics/statistical-review-of-world-energy/downloads.html
  energy_file = system.file(file.path("kaya_tool", "data", "bp-statistical-review-of-world-energy-2017-underpinning-data.xlsx"),
                            package = "kayatool")


  fuel_levels <- c('coal', 'gas', 'oil', 'nuclear', 'renewables', 'total')
  fuel_labels <- c('Coal', 'Natural Gas', 'Oil', 'Nuclear', 'Renewables', 'Total')

  sheet = "Primary Energy - Cons by fuel"

  countries = read_excel(energy_file,  sheet, range = 'A3:A94',
                         na = c('-', '', 'NA', 'N/A', 'na', 'n/a')) %>%
    clean_names() %>%
    rename(country = million_tonnes_oil_equivalent)

  year1 = read_excel(energy_file, sheet, range = "H2", col_names = FALSE) %>%
    simplify() %>% unname()

  ebf1 = read_excel(energy_file, sheet, range = 'B3:H94',
                    na = c('-', '', 'NA', 'N/A', 'na', 'n/a')) %>%
    clean_names() %>%
    mutate(year = year1)

  year2 = read_excel(energy_file, sheet, range = "O2", col_names = FALSE) %>%
    simplify() %>% unname()

  ebf2 = read_excel(energy_file, sheet, range = 'I3:O94',
                              na = c('-', '', 'NA', 'N/A', 'na', 'n/a')) %>%
    clean_names() %>%
    mutate(year = year2)

  ebf = bind_rows(ebf1, ebf2) %>%
    rename(gas = natural_gas, nuclear = nuclear_energy, hydro = hydro_electric,
           renewables = renew_ables) %>%
    mutate(renewables = renewables + hydro) %>%
    select(year, oil, gas, coal, nuclear, renewables) %>%
    mutate_at(vars(-year), funs(. * mtoe))

  energy_by_fuel = bind_rows(countries, countries) %>% bind_cols(ebf) %>%
    filter(!is.na(country)) %>%
    mutate(country = str_replace_all(country, bp_translations)) %>%
    gather(key = fuel, value = quads, -country, -year) %>%
    mutate(fuel = ordered(fuel, levels = fuel_levels, labels = fuel_labels),
           quads = ifelse(is.na(quads), 0.0, quads)) %>%
    group_by(country, year) %>% mutate(pct = 100 * quads / sum(quads, na.rm = T)) %>%
    ungroup()

  invisible(energy_by_fuel)
}

load_energy_by_fuel <- function() {
  e_by_f_file <- "data/energy_by_fuel.Rds"
  if (file.exists(e_by_f_file)) {
    energy_by_fuel <- read_rds(e_by_f_file)
  } else {
    energy_by_fuel <- prepare_energy_by_fuel()
    write_rds(energy_by_fuel, e_by_f_file, compress = "bz2")
  }
  invisible(energy_by_fuel)
}
