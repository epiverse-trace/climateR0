# Vector and pathogen code reference
# From Mordecai et al 2019

if (!require("here")) install.packages("here")
library(here)

vector_reference <- read.csv(here("data-raw", "vector-pathogen_reference.csv"))

usethis::use_data(vector_reference, internal = TRUE, overwrite = TRUE, compress = "bzip2")

