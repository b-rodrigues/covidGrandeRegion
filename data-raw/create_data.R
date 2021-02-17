library(dplyr)
library(lubridate)
library(covidGrandeRegion)

covid_data <- get_greater_region_data()

covid_data <- covid_data %>%
  filter(day < ymd("2021-01-01"))

usethis::use_data(covid_data)
