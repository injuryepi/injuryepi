add_age75 <- function(data, age = age){

# 0–9, 10–17, 18–24, 25–34, 35–44, 45–54, 55–64, 65–74, and 75+ years
  suppressWarnings(suppressMessages(require(classInt)))
  suppressWarnings(suppressMessages(require(dplyr)))
  suppressWarnings(suppressMessages(require(forcats)))

  age <- enquo(age)

  age <- data %>% pull(age)

  agecut75 <- c(0, 9, 17, 24,34,44,54,64, 74, max( age, na.rm = T))

  int75 <- classIntervals(age, n = 9, style="fixed", fixedBreaks=agecut75, intervalClosure = "right")

  agegrp75 <- as.factor(findCols(int75))

  mutate(data,
         agegrp75 = agegrp75,
         age75 = fct_recode(agegrp75, "<10" = "1",
                            "`10-17" = "2",
                            "18-24" = "3",
                            "25-34" = "4",
                            "35-44" = "5",
                            "45-54" = "6",
                            "55-64" = "7",
                            "65-74" = "8",
                            "75+" = "9"))

}
