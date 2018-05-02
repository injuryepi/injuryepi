# https://www.census.gov/geo/reference/codes/cou.html

library(tidyverse)
wa_counties_fips <- read_csv(file = "https://www2.census.gov/geo/docs/reference/codes/files/st53_wa_cou.txt", col_names = F)

wa_counties_fips <- wa_counties_fips %>%
  set_names(
    c("STATE", "STATEFP", "COUNTYFP", "COUNTYNAME", "CLASSFP")
  ) %>%
  set_names(tolower)

wa_counties_fips <- wa_counties_fips %>%
  mutate(countyname = stringr::str_trim(
    gsub("County$", "", countyname))) %>%
  select(countyname, countyfp) %>%
  set_names(c("county", "county_fips"))

wa_counties_fips <- wa_counties_fips %>%
  add_column(county_wa_code =
               stringr::str_pad(1:39, width = 2, side = "l", pad = 0))

devtools::use_data(wa_counties_fips , compress = "xz", overwrite = T)


