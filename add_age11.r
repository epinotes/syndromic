add_age11 <- function(data, age){
  suppressWarnings(suppressMessages(require(classInt)))
  suppressWarnings(suppressMessages(require(dplyr)))
  suppressWarnings(suppressMessages(require(forcats)))
  age <- enquo(age)
  age <- data %>% pull(!!age) %>% unlist()
  age_max <- ifelse(max(age, na.rm = T) > 84, max(age, na.rm = T), 120)
  agecut11 <- c(0, 0.99, 4, 14, 24, 34, 44, 54, 64, 74, 84, age_max)
  int11 <- classIntervals(age, n = 11, style = "fixed", 
                          fixedBreaks = agecut11, intervalClosure = "right")
  agegrp11 <- as.factor(findCols(int11))
  data %>% mutate(agegrp11 = agegrp11, age11 = fct_recode(agegrp11, 
                                                          `<1` = "1", 
                                                          `01-04` = "2", 
                                                          `05-14` = "3", 
                                                          `15-24` = "4", 
                                                          `25-34` = "5", 
                                                          `35-44` = "6",
                                                          `45-54` = "7", 
                                                          `55-64` = "8", 
                                                          `65-74` = "9", 
                                                          `75-84` = "10",
                                                          `85+` = "11")) %>% 
    mutate(agegrp11 = as.double(as.character(agegrp11)))
}
