add_inc_rate <- function(dat, case,pop, digit = 1, naming = "_crude_rate", method = "exact", s = 100000){
  require(epiR, quietly = T)
  dat_inc.rate <- round(epiR::epi.conf(as.matrix(dat[, c(case,pop)]), ctype = "inc.rate", method = method)*s, digits = digit)
  #dat_inc.rate <-  subset(dat_inc.rate, select = -se)
  names(dat_inc.rate) <- paste0(names(dat_inc.rate),naming)
  dat_inc.rate <- dplyr::bind_cols(dat, dat_inc.rate)
  dat_inc.rate
}