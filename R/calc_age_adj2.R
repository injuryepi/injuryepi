calc_age_adj2 <- function(count, pop, s = 100000, r = 1){
  require(epitools)
  require(tidyverse)
  
  us2000std = tibble(us2000std = c(0.013818,0.055317,0.145565,0.138646, 0.135573,0.162613,0.134834,0.087247,0.066037,0.044842,0.015508))
  
  tab = round(epitools::ageadjust.direct(count = count, pop = pop, stdpop = us2000std[,1])*s, r)
  names(tab) = c("crude_rate", "age_adj_rate", "lower_age_adj", "upper_age_adj")
  tab[-1]
  
}