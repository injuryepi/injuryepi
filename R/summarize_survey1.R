summarize_survey1 <- function(data, formula, names, round = 1, level = 0.95){  
  # formula = ~var with var being the variable of interest
  require(dplyr, quietly = T)
  require(survey, quietly = T)
  # var <- eval(substitute(var),data, parent.frame())
  tab.t <- as.data.frame(svytotal(formula, design = data,na.rm = T))
  tab.m <- as.data.frame(svymean(formula, design = data,na.rm = T))
  z <- -qnorm((1-level)/2)
  tab <- data.frame(counts=round(tab.t[,1]),
                    pct=round(100*tab.m[,1],round),
                    ME=round(z*100*tab.m[,2],round))
  tab <- add_column(tab, response = names, .before = T)
  print(tab)
}