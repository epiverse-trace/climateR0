# Fiji dengue case study data
# From Kucharski et al 2018, https://github.com/adamkucharski/fiji-denv3-2014

if (!require("here")) install.packages("here")
library(here)
if (!require("lubridate")) install.packages("lubridate")
library(lubridate)
library(dplyr)
library(tidyr)
library(cinterpolate)

fiji_denv3_2014 <- read.csv(here("data-raw", "fiji-central_lab-tested_denv3-2014.csv")) %>% 
  dplyr::mutate(month_date = lubridate::floor_date(as.Date(date, format = "%d/%m/%Y"), unit = "month")) %>% 
  tidyr::pivot_longer(c("central", "western", "north", "east"), names_to = "geography", values_to = "cases")

fiji_climate <- read.csv(here("data-raw", "fiji_climate.csv")) %>% 
  dplyr::mutate(date = as.Date(date, format = "%d/%m/%Y")) %>% 
  dplyr::rename(month_date = date) %>% 
  dplyr::select(-week)

# Harmonize climate and denv case data

fiji_2014 <- merge(fiji_denv3_2014, fiji_climate, by = "month_date") %>% 
  dplyr::mutate(date = as.Date(date,  format = "%d/%m/%Y"), year = lubridate::year(date)) %>% 
  # filter so dataset only contains data from Central division
  dplyr::filter(geography == "central") %>% 
  dplyr::select(date, cases, av_temp) %>% 
  dplyr::mutate(year = lubridate::year(date))

# Add population data

# Linear interpolation for population between census data points

fiji_census <- read.csv(here("data-raw", "fiji_census_2007_2017.csv")) %>% 
  tidyr::pivot_longer(cols = c(X2007, X2017), names_to = "year", values_to = "population") %>% 
  dplyr::mutate(year = gsub("[^0-9]", "", year)) %>% 
  dplyr::mutate(year = as.numeric(year), population = as.numeric(population)) %>% 
  dplyr::group_by(division, year) %>% 
  dplyr::summarise(population = sum(population))

fiji_pop <- data.frame()
for(div in unique(fiji_census$division)){
  pop_out <- 
    fiji_sub <- fiji_census %>% filter(division == div)
  pop_fun <- cinterpolate::interpolation_function(fiji_sub$year, fiji_sub$population, type = "linear")
  
  pop_out <- data.frame(
    year = 2007:2017,
    division = div,
    pop = round(pop_fun(2007:2017)))
  
  fiji_pop <- rbind(fiji_pop, pop_out)
  
}
rm(pop_out)

fiji_pop <- fiji_pop %>% 
  dplyr::rename(geography = division) %>% 
  dplyr::filter(geography == "central") %>% 
  dplyr::select(year, pop)

# Add population to fiji data frame

fiji_2014 <- merge(fiji_2014, fiji_pop, by = c("year"), all.x = TRUE) %>% 
  dplyr::select(date, cases, av_temp, pop)

usethis::use_data(fiji_2014,  compress = "xz", overwrite = TRUE)

