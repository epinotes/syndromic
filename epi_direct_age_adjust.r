epi_direct_age_adjust <- function(data, agegrp = agegrp11, count = count, population = population, s = 100000, r = 1, alpha = 0.05 ){
  
  agegrp <- enquo(agegrp)
  age_name <- quo_name(agegrp)
  count <- enquo(count)
  count_name <- quo_name(count)
  population <- enquo(population)
  pop_name <- quo_name(population)
  
  
  suppressWarnings(suppressMessages(require(dplyr)))
  suppressWarnings(suppressMessages(require(tibble)))
  suppressWarnings(suppressMessages(require(tidyr)))
  
  us2000std = tibble(std_pop = c(0.013818,0.055317,0.145565,0.138646, 0.135573,0.162613,0.134834,0.087247,0.066037,0.044842,0.015508),
                     !!age_name := as.double(c(1:11)))
  
  
  # calculate adjusted rate and confidence interval with gamma distribution
  # modified from the R package
  # https://cran.r-project.org/web/packages/epitools/index.html 
  # with more method details from
  # https://pdfs.semanticscholar.org/584d/0d020d77e84d193f42e162c59c64795dac6c.pdf
  
  # suppressWarnings(suppressMessages(
  
  data <- data %>% as.data.frame() %>% 
    mutate(!!age_name := as.double(as.character(!!agegrp))) %>%
    right_join(us2000std, by = age_name) %>% 
    mutate(count_name := replace_na(!!count, 0),
           pop_name := replace_na(!!population, 0))
  
  std_pop <- data %>%
    pull(std_pop) %>% unlist
  
  pop_v <- data %>% pull(as.double(!!population)) %>% unlist
  count_v <- data %>% pull(as.double(!!count)) %>% unlist
  
  rate <- count_v / pop_v
  stdwt <- std_pop / sum(std_pop)
  dsr <- sum(stdwt * rate)
  var_k <- count_v / (pop_v)^2
  dsr_var <- sum((stdwt^2) * var_k)
  
  wm <- max(stdwt/pop_v)
  gamma_lci <- qgamma(alpha/2, shape = (dsr^2)/dsr_var, scale = dsr_var/dsr)
  gamma_uci <- qgamma(1 - alpha/2, 
                      shape = ((dsr + wm)^2)/(dsr_var + wm^2), 
                      scale = (dsr_var + wm^2)/(dsr + wm))
  
  tibble(count = sum(count_v),
         population = sum(pop_v),
         age_adj_rate = round(dsr*s, r),
         lower_age_adj = round(gamma_lci*s, r),
         upper_age_adj = round(gamma_uci*s, r))
}
