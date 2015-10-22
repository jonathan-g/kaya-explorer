library(readxl)
library(magrittr)
library(dplyr)
library(tidyr)
library(stringr)

mtoe = 1 / 25.2 # quads
data.dir <- 'data'

load_kaya <- function() {
  population <- read.csv(file.path(data.dir, 'HistoricalPopulationValues.csv'), skip=12, header=T, stringsAsFactors=F)
  countries <- population[,1:4]
  countries[is.na(countries)] <- ''
  countries <- unlist(lapply(seq_len(nrow(countries)), function(i) str_trim(paste(countries[i,], collapse=''))))

  population <- cbind(country = countries, population[,-(1:4)])
  population  %<>% gather(key = "year", value = "population", -country) %>%
    mutate(year = as.numeric(str_sub(year,2)), population = population / 1E+9)

  gdppc <- read.csv(file.path(data.dir, 'historicalRealPerCapitaGDPValues.csv'), header=T, skip=12, stringsAsFactors = F)
  countries <- gdppc[,1:4]
  countries[is.na(countries)] <- ''
  countries <- unlist(lapply(seq_len(nrow(countries)), function(i) str_trim(paste(countries[i,], collapse=''))))

  gdppc <- cbind(country = countries, gdppc[,-(1:4)])
  gdppc %<>% gather(key = year, value = gdp.pc, -country) %>%
    mutate(year = as.numeric(str_sub(year,2)), gdp.pc = gdp.pc / 1000) %>%
    na.omit()

  gdppc <- merge(population, gdppc) %>% na.omit()
  gdppc$country <- str_replace(str_trim(as.character(gdppc$country)), "^(Total|Republic of) +","")

  energy <- read_excel(file.path(data.dir, 'bp-statistical-review-of-world-energy-2015-workbook.xlsx'),
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

  carbon <- read_excel(file.path(data.dir, 'bp-statistical-review-of-world-energy-2015-workbook.xlsx'),
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
           f = carbon.emissions / energy.consumption)

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
  kaya <- energy.intensity %>% select(year, country,
                                      P = population,
                                      g = gdp.pc,
                                      e, f,
                                      E = energy.consumption,
                                      F = carbon.emissions) %>%
    mutate(ef = e * f, G = P * g)
  list(kaya = kaya, data = energy.intensity)
}
