add_age4 <- function(data, age = age){
  
  suppressWarnings(suppressMessages(require(classInt)))
  suppressWarnings(suppressMessages(require(dplyr)))
  suppressWarnings(suppressMessages(require(forcats)))
  
  age <- enquo(age)
  
  age <- data %>% pull(age)
  
  agecut4 <- c(0, 24,44, 64, max(age, na.rm = T))
  int4 <- classIntervals(age, n = 4, style="fixed", fixedBreaks=agecut4, intervalClosure = "right")
  
  agegrp4 <- as.factor(findCols(int4))
  
  mutate(data, 
         agegrp4 = agegrp4,
         age4 = fct_recode(agegrp4, "<25" = "1", 
                            "25-44" = "2",
                            "45-64" = "3",
                            "65+"   = "4"))
  
  
}
