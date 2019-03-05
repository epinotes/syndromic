first_valid_code <- function(data, colvec, pattern){
# colvec: the columns of interest
# pattern: the regex pattern to look for
  colvec = enquo(colvec)
  f0 <- function(x) grepl(pattern = pattern, x, ignore.case = T, perl = T)
  f1 <- function(x) detect(x, f0)
  data %>% 
    select(!!colvec) %>%
    map_dfr(as.character) %>% 
    transpose %>% map(f1) %>% map_if(is.null, ~ NA_character_) %>% 
  unlist
 }
