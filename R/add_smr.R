add_smr <- function(dat, cols, digit = 2, naming = "_smr"){
  # two elements vector of respectively case column and pop column numbers
  # example c(2,3)
  require(epiR, quietly = T)
  dat_smr <- round(epiR::epi.conf(as.matrix(dat[, cols]), ctype = "smr"), digits = digit)
  dat_smr <-  subset(dat_smr, select = -se)
  names(dat_smr) <- paste0(names(dat_smr),naming)
  dat_smr <- cbind(dat, dat_smr)
  dat_smr
}