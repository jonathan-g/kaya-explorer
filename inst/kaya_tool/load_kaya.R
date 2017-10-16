library(magrittr)
library(tidyverse)
library(readxl)
library(stringr)


mtoe = 1 / 25.2 # quads

load_kaya <- function() {
  population <- read.csv(system.file(file.path("kaya_tool", "data", "HistoricalPopulationValues.csv"),
                                     package = "kayatool"), skip=12, header=T, stringsAsFactors=F)
  countries <- population[,1:4]
  countries[is.na(countries)] <- ''
  countries <- unlist(lapply(seq_len(nrow(countries)),
                             function(i) str_trim(paste(countries[i,], collapse=''))))

  population <- cbind(country = countries, population[,-(1:4)])
  population  %<>% gather(key = "year", value = "population", -country) %>%
    mutate(year = as.numeric(str_sub(year,2)), population = population / 1E+9)

  gdppc <- read.csv(system.file(file.path("kaya_tool", "data", "historicalRealPerCapitaGDPValues.csv"),
                                package = "kayatool"), header=T, skip=12, stringsAsFactors = F)
  countries <- gdppc[,1:4]
  countries[is.na(countries)] <- ''
  countries <- unlist(lapply(seq_len(nrow(countries)), function(i) str_trim(paste(countries[i,], collapse=''))))

  gdppc <- cbind(country = countries, gdppc[,-(1:4)])
  gdppc %<>% gather(key = year, value = gdp.pc, -country) %>%
    mutate(year = as.numeric(str_sub(year,2)), gdp.pc = gdp.pc / 1000) %>%
    na.omit()

  gdppc <- merge(population, gdppc) %>% na.omit()
  gdppc$country <- str_replace(str_trim(as.character(gdppc$country)), "^(Total|Republic of) +","")

  energy <- read_excel(system.file(file.path("kaya_tool", "data", "bp-statistical-review-of-world-energy-2015-workbook.xlsx"),
                                   package = "kayatool"),
                       'Primary Energy Consumption ', skip=2)
  energy <- energy[,1:(ncol(energy)-3)]
  names(energy)[1] <- 'country'
  #
  # Quads of primary energy consumed
  #
  energy %<>% filter(! is.na(country)) %>%
    gather(key = year, value = energy.consumption, -country) %>%
    filter(! is.na(energy.consumption) & ! str_detect(energy.consumption, 'n/a')) %>%
    mutate(year = as.numeric(as.character(year)),
           energy.consumption = as.numeric(energy.consumption) * mtoe)

  carbon <- read_excel(system.file(file.path("kaya_tool", "data", "bp-statistical-review-of-world-energy-2015-workbook.xlsx"),
                                   package = "kayatool"),
                       'Carbon Dioxide Emissions', skip=2)
  carbon <- carbon[,1:(ncol(carbon)-3)]
  names(carbon)[1] <- 'country'
  #
  # Million tonnes carbon
  #
  carbon %<>% filter(! is.na(country)) %>%
    gather(key = year, value = carbon.emissions, -country) %>%
    filter(! is.na(carbon.emissions) & ! str_detect(carbon.emissions, 'n/a')) %>%
    mutate(year = as.numeric(as.character(year)),
           carbon.emissions = as.numeric(carbon.emissions) * 12 / (12 + 32))

  energy <- merge(energy, carbon)

  energy %<>% mutate(country = str_replace(str_trim(country),"^(Total|Republic of) +",""))

  if (file.exists(system.file(file.path("kaya_tool", "data", "sri_lanka.csv"), package = "kayatool"))) {
    sl <- read_csv(system.file(file.path("kaya_tool", "data", "sri_lanka.csv"), package = "kayatool")) %>%
      gather(key = Year, value = value, -c(Variable:Unit)) %>%
      mutate(Year = as.integer(Year),
             Variable = str_replace_all(Variable, c("CO2 Emissions" = 'carbon.emissions',
                                                    "Primary Energy Production" = "energy.production",
                                                    "Primary Energy Consumption" = "energy.consumption"))
      ) %>%
      select(-Unit) %>%
      rename_all(funs(str_to_lower(.))) %>%
      spread(key = variable, value = value) %>%
      mutate(carbon.emissions = carbon.emissions * 12 / (12 + 32)) %>%
      select(country, year, carbon.emissions, energy.consumption)


    energy %<>% bind_rows(sl)
  }

  dict1 <- c('US' = "United States")
  dict2 <- c('Asia and Oceania' = 'Asia Pacific')

  indices <- which(energy$country %in% names(dict1))
  energy$country[indices] <- dict1[energy$country[indices]]

  indices <- which(gdppc$country %in% names(dict2))
  gdppc$country[indices] <- dict2[gdppc$country[indices]]


  #
  # Quads per trillion dollars
  #
  energy.intensity <- merge(energy, gdppc) %>%
    mutate(energy.pc = energy.consumption / population,
           carbon.pc = carbon.emissions / population,
           e = energy.pc / gdp.pc,
           f = carbon.emissions / energy.consumption,
           country = factor(country))

  exclude_countries <- c('Other Middle East')
  energy.intensity <- filter(energy.intensity, ! country %in% exclude_countries)

  world <- c('World')
  regions <- c('Africa', 'Asia Pacific', 'Middle East', 'Outher Middle East',
               'North America')

  energy.intensity$geography <- 'country'
  energy.intensity$geography[energy.intensity$country %in% regions] <- 'region'
  energy.intensity$geography[energy.intensity$country %in% world] <- 'world'
  energy.intensity$geography <- ordered(energy.intensity$geography,
                                        levels = c('country', 'region', 'world'))

  energy.intensity <- arrange(energy.intensity, desc(geography), country, year)

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
  kaya <- energy.intensity %>% select(year, country, geography,
                                      P = population,
                                      g = gdp.pc,
                                      e, f,
                                      E = energy.consumption,
                                      F = carbon.emissions) %>%
    mutate(ef = e * f, G = P * g)

  list(kaya = kaya, data = energy.intensity)
}

translate_nations <- function(df) {
  nation_table = data_frame(
    nation = c( "United States", "Canada",
                "Mexico/Chile", "Mexico/Chile",
               "Japan", "South Korea",
               "Australia/New Zealand", "Australia/New Zealand",
               "Russia", "China", "India",
               "Sri Lanka",
               "Brazil", "Total World"),
    country = c("United States", "Canada",
                "Mexico", "Chile",
                "Japan", "South Korea",
                "Australia", "New Zealand",
                "Russia", "China", "India",
                "Sri Lanka",
                "Brazil", "World")
  )

  inner_join(nation_table, df, by = 'nation') %>%
    select(-nation) %>%
    invisible()
}

load_top_down <- function() {
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
    full_join(e, by = 'nation') %>%
    translate_nations()
  invisible(top.down)
}

export_kaya <- function(kaya, country) {
  x <- country
  data <- kaya %>% filter(country == x) %>%
    select(Year = year, P,G,E,F) %>%
    mutate(P = P * 1E+6, G = G * 1E+6, F = F * 44 / 12)
  filename <- paste(gsub(" +", "_", country), '.csv')
  write.csv(data, file = filename, row.names=FALSE)
}

