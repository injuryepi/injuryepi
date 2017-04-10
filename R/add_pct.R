add_pct <- function(data, case, pop ,level=.95, s = 100 ,digit = 1, naming = "_pct"){
  # case: numerator column number
  # pop: denominator column number
  require(dplyr, quietly = T)
  x <- data[, case] 
  n <- data[, pop]
  est <- x/n
  z <- -qnorm((1-level)/2)
  moe <- ((z/sqrt(n))*sqrt(est*(1-est) + z^2/(4*n)))/(1+z^2/n)
  center <- (est+z^2/(2*n))/(1+z^2/n)
  est <- round(est*s, digit)
  lcl <- center - moe
  ucl <- center + moe
  lower <- round(lcl*s, digit)
  upper <- round(ucl*s,digit)
  ci <- data.frame(est, lower, upper)
  names(ci) <- paste0(names(ci), naming)
  dat_pct <- dplyr::bind_cols(data, ci)
  dat_pct
}