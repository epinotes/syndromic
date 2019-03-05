first_valid_idx <- function(data, colvec, pattern){
  #colvec: the columns of interest
  # pattern: the regex pattern to match
  colvec = enquo(colvec)
  f0 <- function(x) grepl(pattern = pattern, x, ignore.case = T, perl = T)
  f1 <- function(x) detect_index(x, f0)
  data %>% select(!!colvec) %>% 
    map_dfr(as.character) %>% transpose %>% 
    map_int(f1)
}
