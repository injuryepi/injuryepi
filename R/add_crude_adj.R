add_crude_adj <- function(x, y){

  require(foreach, quietly = T)
  require(epiR, quietly = T)
  require(tidyverse)

  # crude rates

  add_inc_rate <- function(dat, case,pop, digit = 1, naming = "_crude_rate", method = "exact", s = 100000){
    dat_inc.rate <- round(epiR::epi.conf(as.matrix(dat[, c(case,pop)]), ctype = "inc.rate", method = method)*s, digits = digit)
    #dat_inc.rate <-  subset(dat_inc.rate, select = -se)
    names(dat_inc.rate) <- paste0(names(dat_inc.rate),naming)
    dat_inc.rate <- dplyr::bind_cols(dat, dat_inc.rate)
    dat_inc.rate
  }
  dat_a0 <- x %>% group_by(year) %>% summarise_each(funs(sum), count, pop)
  a <- grep("count|pop", names(dat_a0))
  dat_a0 <- add_inc_rate(dat_a0, case = a[1], pop = a[2])

  # add age-adjusted rates

  calc_age_adj2 <- function(count, pop, s = 100000, r = 1){
    require(epitools)
    us2000std = tibble(us2000std = c(0.013818,0.055317,0.145565,0.138646, 0.135573,0.162613,0.134834,0.087247,0.066037,0.044842,0.015508))

    tab = round(epitools::ageadjust.direct(count = count, pop = pop, stdpop = us2000std[,1])*s, r)
    names(tab) = c("crude_rate", "age_adj_rate", "lower_age_adj", "upper_age_adj")
    tab[-1]

  }

  b <- grep("count|pop", names(x))

  dat_adj <- foreach(i= y,.combine = rbind) %do% {
    calc_age_adj2(count = filter(x, year == i ) %>% select(b[1]),
                  pop = filter(x, year == i ) %>% select(b[2]))
  }

  bind_cols(dat_a0, as.data.frame(dat_adj))

}
