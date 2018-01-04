add_age9 <- function(data, age = age){
  
  suppressWarnings(suppressMessages(require(classInt)))
  suppressWarnings(suppressMessages(require(dplyr)))
  suppressWarnings(suppressMessages(require(forcats)))
  
  age <- enquo(age)
  
  age <- data %>% pull(!!age)
  
  agecut9 <- c(0,.99, 4, 14,24,34,44,54,64, max(age, na.rm = T))
  int9 <- classIntervals(age, n = 9, style="fixed", fixedBreaks=agecut9, intervalClosure = "right")
  
  agegrp9 <- as.factor(findCols(int9))
  
  mutate(data, 
         agegrp9 = agegrp9,
         age9 = fct_recode(agegrp9, "<1" = "1", 
                            "01-4"  = "2",
                            "05-14" = "3",
                            "15-24" = "4",
                            "25-34" = "5",
                            "35-44" = "6",
                            "45-54" = "7",
                            "55-64" = "8",
                            "65+"   = "9"))
  
  
}
