add_age10 <- function(data, age = age){

  suppressWarnings(suppressMessages(require(classInt)))
  suppressWarnings(suppressMessages(require(dplyr)))
  suppressWarnings(suppressMessages(require(forcats)))

  age <- enquo(age)

  age <- data %>% pull(age)

  agecut10 <- c(0, 9, 14,24,34,44,54,64,74,84, max(age, na.rm = T))
  int10 <- classIntervals(age, n = 10, style="fixed", fixedBreaks=agecut10, intervalClosure = "right")

  agegrp10 <- as.factor(findCols(int10))

  mutate(data,
         agegrp10 = agegrp10,
         age10 = fct_recode(agegrp10, "<10" = "1",
                            "10-14"  = "2",
                            "15-24" = "3",
                            "25-34" = "4",
                            "35-44" = "5",
                            "45-54" = "6",
                            "55-64" = "7",
                            "65-74" = "8",
                            "75-84" = "9",
                            "85+"   = "10"))


}
