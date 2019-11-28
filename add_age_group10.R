add_age_group10 <- function(data, age){
  suppressWarnings(suppressMessages(require(classInt)))
  suppressWarnings(suppressMessages(require(dplyr)))
  suppressWarnings(suppressMessages(require(forcats)))
  age <- data %>% pull({{age}}) %>% unlist()
  age_max <- ifelse(max(age, na.rm = T) > 84, max(age, na.rm = T), 120)
  agecut10 <- c(0, 10, 14, 24, 34, 44, 54, 64, 74, 84, age_max)
  int10 <- classIntervals(age, n = 10, style = "fixed", 
                          fixedBreaks = agecut10, intervalClosure = "right")
  agegrp10 <- as.factor(findCols(int10))
  data %>% mutate(agegrp10 = agegrp10, 
                  age_group10 = fct_recode(agegrp10,
                                           `0-10` = "1",
                                           `11-14` = "2",
                                           `15-24` = "3",
                                           `25-34` = "4",
                                           `35-44` = "5",
                                           `45-54` = "6",
                                           `55-64` = "7",
                                           `65-74` = "8",
                                           `75-84` = "9",
                                           `85+` = "10")) %>% 
    mutate(age_group10 = fct_explicit_na(age_group10)) %>% 
    select(-agegrp10)
}
