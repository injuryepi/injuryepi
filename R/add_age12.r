add_age12 <- function(data, age = age){
  
  suppressWarnings(suppressMessages(require(classInt)))
  suppressWarnings(suppressMessages(require(dplyr)))
  suppressWarnings(suppressMessages(require(forcats)))
  
  age <- enquo(age)
  
  age <- data %>% pull(age)
  
  agecut12 <- c(0, 9, 14,17, 19, 24,34,44,54,64, 74, 84, max( age, na.rm = T))
  
  int12 <- classIntervals(age, n = 12, style="fixed", fixedBreaks=agecut12, intervalClosure = "right")
  
  agegrp12 <- as.factor(findCols(int12))
   
  mutate(data, 
         agegrp12 = agegrp12,
         age12 = fct_recode(agegrp12, "<10" = "1", 
                                  "`10-14" = "2",
                                  "15-17" = "3",
                                  "18-19" = "4",
                                  "20-24" = "5",
                                  "25-34" = "6",
                                  "35-44" = "7",
                                  "45-54" = "8",
                                  "55-64" = "9",
                                  "65-74" = "10",
                                  "75-84" = "11",
                                  "85+"   = "12"))
  
}
