remove_n2m <- function(x, n = 1, m = 9){
require(dplyr)
  ifelse(between(x, n, m), NA, x)
}

# data[, cols] <- sapply(data[, cols], remove_n2m)
