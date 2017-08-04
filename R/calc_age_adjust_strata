calc_age_adjust_strata <- function(data, agegrp, count, population, ... ){
grp_by <- quos(...)
agegrp <- enquo(agegrp)
count <- enquo(count)
population <- enquo(population)

suppressWarnings(suppressMessages(require(tidyverse)))
suppressWarnings(suppressMessages(require(epitools)))
suppressWarnings(suppressMessages(require(foreach)))

data1 <-  data %>% select(!!!grp_by, !!count, !!agegrp, !!population) %>% 
  mutate(id = paste0(!!!grp_by))
uid <- data1 %>% pull(id) %>% unique()

data0 <- data %>% group_by(!!!grp_by) %>% 
  summarise_at(vars(!!count, !!population), sum) %>% 
  ungroup() %>% mutate(uid = paste0(!!!grp_by))

calc_age_adj2 <- function(count, population, s = 100000, r = 1){
  us2000std = tibble(us2000std = c(0.013818,0.055317,0.145565,0.138646, 0.135573,0.162613,0.134834,0.087247,0.066037,0.044842,0.015508))
  
  tab = round(epitools::ageadjust.direct(count = count, pop = population, stdpop = us2000std[,1])*s, r)
  names(tab) = c("crude_rate", "age_adj_rate", "lower_age_adj", "upper_age_adj")
  tab[-1]
  
}

dat_adj <- foreach(i = uid,.combine = rbind) %do% {
  calc_age_adj2(count = filter(data1, id == i ) %>% select(!!count),
                population = filter(data1, id == i ) %>% select(!!population))
}
data2 <- as_tibble(dat_adj) %>% add_column(uid = uid, .before = 1)

data0 %>% right_join(data2, by = "uid") %>% select(-uid)

}
