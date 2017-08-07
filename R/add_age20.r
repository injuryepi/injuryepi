add_age20 <- function(data, age = age){
  
  suppressWarnings(suppressMessages(require(classInt)))
  suppressWarnings(suppressMessages(require(dplyr)))
  suppressWarnings(suppressMessages(require(forcats)))
  
  age <- enquo(age)
  
  age <- data %>% pull(age)
  
  agecut20 <- c(0,.99, 4, 9, 14,17, 19, 24, 29, 34, 39, 44,49, 54,59,64,69,74,79,84, max(age, na.rm = T))
  int20 <- classIntervals(age, n = 20, style="fixed", fixedBreaks=agecut20, intervalClosure = "right")
  
agegrp20 <- as.factor(findCols(int20))
  
mutate(data, 
       agegrp20 = agegrp20,
       age20 = fct_recode(agegrp20, "<1" = "1",
                              "01_04" = "2",
                              "05_09" = "3",
                              "10_14" = "4",
                              "15_17" = "5",
                              "18_19" = "6",
                              "20_24" = "7",
                              "25_29" = "8",
                              "30_34" = "9",
                              "35-39" = "10",
                              "40_44" = "11",
                              "45-49" = "12",
                              "50_54" = "13",
                              "55_59" = "14",
                              "60_64" = "15",
                              "65-69" = "16",
                              "70_74" = "17",
                              "75-79" = "18",
                              "80_84" = "19",
                              "85+"   = "20"))
  
}
