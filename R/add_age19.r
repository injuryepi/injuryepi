add_age19 <- function(data, age = age){
  
  suppressWarnings(suppressMessages(require(classInt)))
  suppressWarnings(suppressMessages(require(dplyr)))
  suppressWarnings(suppressMessages(require(forcats)))
  
  age <- enquo(age)
  
  age <- data %>% pull(!!age)
  
agecut19 <- c(0,.99, 4, 9, 14,19, 24, 29, 34, 39, 44,49, 54,59,64,69,74,79,84, max(age, na.rm = T))
  int19 <- classIntervals(age, n = 19, style="fixed", fixedBreaks=agecut19, intervalClosure = "right")
  
agegrp19 <- as.factor(findCols(int19))
  
mutate(data, 
       agegrp19 = agegrp19,
       age19 = fct_recode(agegrp19, "<1" = "1",
                              "01_04" = "2",
                              "05_09" = "3",
                              "10_14" = "4",
                              "15_19" = "5",
                              "20_24" = "6",
                              "25_29" = "7",
                              "30_34" = "8",
                              "35-39" = "9",
                              "40_44" = "10",
                              "45-49" = "11",
                              "50_54" = "12",
                              "55_59" = "13",
                              "60_64" = "14",
                              "65-69" = "15",
                              "70_74" = "16",
                              "75-79" = "17",
                              "80_84" = "18",
                              "85+"   = "19"))
  
}
