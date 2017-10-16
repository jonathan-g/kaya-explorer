library(tidyverse)
library(stringr)

load_energy_by_fuel <- function() {

  coal_filename = system.file(file.path("kaya_tool", "data", "World_coal_consumption_by_region.csv"), package = "kayatool")
  oil_filename <- system.file(file.path("kaya_tool", "data", "World_liquids_consumption_by_region.csv"), package = "kayatool")
  gas_filename <- system.file(file.path("kaya_tool", "data", "World_natural_gas_consumption_by_region.csv"), package = "kayatool")
  nuclear_filename <- system.file(file.path("kaya_tool", "data", "World_nuclear_energy_consumption_by_region.csv"), package = "kayatool")
  renewable_filename <- system.file(file.path("kaya_tool", "data", "World_renewable_energy_by_region.csv"), package = "kayatool")

  # See US EIA, Annual Energy Review 2012, Appendix A, Table A-6 for conversion factors, esp. nuclear
  # http://www.eia.gov/totalenergy/data/annual/pdf/aer.pdf)
  conversions <- tibble(units = c('quad Btu', 'Tcf', 'bill kWh', 'million b/d'),
                        conversion = c(1, 1.028 , (10452. / 3412.) * 3412. * 1E9 * 1E-15, 365 * 5.8E12 * 1E-15))

  translations <- c('Russia' = 'Former Soviet Union', 'Total World' = 'World') %>%
    setNames(str_c('^', names(.), '$'))

  filename <- coal_filename
  cons <- read_csv(filename, skip=7, col_names = FALSE) %>%
    mutate(X2 = str_replace_all(X2, '"','')) %>%
    tidyr::extract(X2, c('full.name','api.key','units'), "(.*),(.*),(.*)")
  cn <- read_csv(filename, skip=4, col_names = FALSE, n_max = 1) %>%
    mutate(X1 = "country") %>% mutate_all(funs(str_replace_all(.,'[^[:alnum:]._]+','.')))
  colnames(cons) <- cn
  coal <- cons %>% select(-full.name, -api.key) %>% gather(-country,-units, key = year, value = consumption) %>%
    filter(! is.na(units) & str_detect(year, '^[0-9]+$')) %>%
    mutate(year = as.integer(year), consumption = as.numeric(consumption), fuel = 'coal')

  filename <- oil_filename
  cons <- read_csv(filename, skip=7, col_names = FALSE) %>%
    mutate(X2 = str_replace_all(X2, '"','')) %>%
    tidyr::extract(X2, c('full.name','api.key','units'), "(.*),(.*),(.*)")
  cn <- read_csv(filename, skip=4, col_names = FALSE, n_max = 1) %>%
    mutate(X1 = "country") %>% mutate_all(funs(str_replace_all(.,'[^[:alnum:]._]+','.')))
  colnames(cons) <- cn
  oil <- cons %>% select(-full.name, -api.key) %>% gather(-country,-units, key = year, value = consumption) %>%
    filter(! is.na(units) & str_detect(year, '^[0-9]+$')) %>%
    mutate(year = as.integer(year), consumption = as.numeric(consumption), fuel = 'oil')

  filename <- gas_filename
  cons <- read_csv(filename, skip=7, col_names = FALSE) %>%
    mutate(X2 = str_replace_all(X2, '"','')) %>%
    tidyr::extract(X2, c('full.name','api.key','units'), "(.*),(.*),(.*)")
  cn <- read_csv(filename, skip=4, col_names = FALSE, n_max = 1) %>%
    mutate(X1 = "country") %>% mutate_all(funs(str_replace_all(.,'[^[:alnum:]._]+','.')))
  colnames(cons) <- cn
  gas <- cons %>% select(-full.name, -api.key) %>% gather(-country,-units, key = year, value = consumption) %>%
    filter(! is.na(units) & str_detect(year, '^[0-9]+$')) %>%
    mutate(year = as.integer(year), consumption = as.numeric(consumption), fuel = 'gas')

  filename <- nuclear_filename
  cons <- read_csv(filename, skip=7, col_names = FALSE) %>%
    mutate(X2 = str_replace_all(X2, '"','')) %>%
    tidyr::extract(X2, c('full.name','api.key','units'), "(.*),(.*),(.*)")
  cn <- read_csv(filename, skip=4, col_names = FALSE, n_max = 1) %>%
    mutate(X1 = "country") %>% mutate_all(funs(str_replace_all(.,'[^[:alnum:]._]+','.')))
  colnames(cons) <- cn
  nuclear <- cons %>% select(-full.name, -api.key) %>% gather(-country,-units, key = year, value = consumption) %>%
    filter(! is.na(units) & str_detect(year, '^[0-9]+$')) %>%
    mutate(year = as.integer(year),
           consumption = ifelse(is.na(consumption) | consumption == '', '0',consumption) %>% as.numeric(),
           fuel = 'nuclear')


  filename <- renewable_filename
  cons <- read_csv(filename, skip=7, col_names = FALSE) %>%
    mutate(X2 = str_replace_all(X2, '"','')) %>%
    tidyr::extract(X2, c('full.name','api.key','units'), "(.*),(.*),(.*)")
  cn <- read_csv(filename, skip=4, col_names = FALSE, n_max = 1) %>%
    mutate(X1 = "country") %>% mutate_all(funs(str_replace_all(.,'[^[:alnum:]._]+','.')))
  colnames(cons) <- cn
  renewables <- cons %>% select(-full.name, -api.key) %>% gather(-country,-units, key = year, value = consumption) %>%
    filter(! is.na(units) & str_detect(year, '^[0-9]+$')) %>%
    mutate(year = as.integer(year), consumption = as.numeric(consumption), fuel = 'renewables')

  energy <- bind_rows(coal, oil, gas, nuclear, renewables) %>%
    filter(! is.na(units)) %>%
    left_join(conversions, by = 'units') %>% mutate(quads = consumption * conversion) %>%
    select(-conversion) %>%
    group_by(country, year) %>% mutate(pct = 100 * quads / sum(quads)) %>% ungroup() %>%
    arrange(year, country, fuel)

  slashes <- energy %>% filter(  str_detect(country, fixed('/')))
  energy  <- energy %>% filter(! str_detect(country, fixed('/')))

  x <- data.frame()
  while(nrow(slashes) > 0) {
    y <- slashes %>%
      mutate(country = str_replace_all(country, '^([^/]+)/(.*)$','\\1'))
    x <- bind_rows(x, y)
    slashes <- slashes %>%
      filter(str_detect(country, fixed('/'))) %>%
      mutate(country = str_replace_all(country, '^([^/]+)/(.*)$','\\2')) %>%
      filter(country != '')
  }

  fuel_levels <- c('coal', 'gas', 'oil', 'nuclear', 'renewables', 'total')
  fuel_labels <- c('Coal', 'Natural Gas', 'Oil', 'Nuclear', 'Renewables', 'Total')
  energy <- bind_rows(energy, x) %>%
    mutate(country = str_replace_all(country, translations),
           fuel = ordered(fuel, levels = fuel_levels, labels = fuel_labels))

  invisible(energy)
}
