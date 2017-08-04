calc_age_adjust_simple <- function(data, agegrp, count, population, s = 100000, r = 1 ){
  agegrp <- enquo(agegrp)
  count <- enquo(count)
  population <- enquo(population)
  
  suppressWarnings(suppressMessages(require(tidyverse)))
  suppressWarnings(suppressMessages(require(epitools)))

  data1 <-  data %>% select(!!count, !!agegrp, !!population) 
  
  data0 <- data %>% 
    summarise_at(vars(!!count, !!population), sum) 
  
  calc_age_adj2 <- function(count, population, s = s, r = r){
    us2000std = tibble(us2000std = c(0.013818,0.055317,0.145565,0.138646, 0.135573,0.162613,0.134834,0.087247,0.066037,0.044842,0.015508))
    
    tab = round(epitools::ageadjust.direct(count = count, pop = population, stdpop = us2000std[,1])*s, r)
    tab[-1]
    
  }
  
  dat_adj <- calc_age_adj2(count = data1%>% select(!!count),
                  population = data1 %>% select(!!population))
  
  data0 %>% 
    add_column(age_adj_rate = dat_adj[1], lower_age_adj = dat_adj[2], upper_age_adj = dat_adj[3])
  
}
