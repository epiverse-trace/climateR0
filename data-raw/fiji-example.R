# Fiji dengue case study data
# From Kucharski et al 2018, https://github.com/adamkucharski/fiji-denv3-2014

if (!require("here")) install.packages("here")
library(here)
if (!require("lubridate")) install.packages("lubridate")
library(lubridate)
library(dplyr)
library(tidyr)

fiji_denv3_2014 <- read.csv(here("data-raw", "fiji-central_lab-tested_denv3-2014.csv")) |> 
  dplyr::mutate(month_date = lubridate::floor_date(as.Date(date, format = "%d/%m/%Y"), unit = "month")) |> 
  tidyr::pivot_longer(c("central", "western", "north", "east"), names_to = "geography", values_to = "cases")

fiji_climate <- read.csv(here("data-raw", "fiji_climate.csv")) |> 
  dplyr::mutate(date = as.Date(date, format = "%d/%m/%Y")) |> 
  dplyr::rename(month_date = date) |> 
  dplyr::select(-week)

# Harmonize climate and denv case data

fiji_2014 <- merge(fiji_denv3_2014, fiji_climate, by = "month_date") |> 
  dplyr::mutate(date = as.Date(date,  format = "%d/%m/%Y"), year = lubridate::year(date)) |> 
  # filter so dataset only contains data from Central division
  dplyr::filter(geography == "central") |> 
  dplyr::select(date, cases, av_temp)

usethis::use_data(fiji_2014,  compress = "xz", overwrite = TRUE)
