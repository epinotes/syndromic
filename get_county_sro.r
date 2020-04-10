#' get_county_sro
#'
#' @param username 
#' @param password 
#' @param site_no 
#' @param user_id 
#' @param state 
#' @param start_date 
#' @param end_date
#' @param timeresolution 
#'
#' @return
#' @export
#'
#' @examples
#' 

get_county_sro <- function(username, password, site_no, user_id, state, start_date, end_date, timeresolution = "monthly") {
  
  # dates argument in the format "2019-01-31"
  # state argument in the format of quoted two character state name example state = "WA
  
  require(dplyr, quietly = T)
  require(httr, quietly = T)
  require(glue, quietly = T)
  require(purrr, quietly = T)
  require(tidycensus, quietly = T)
  
  start_date = format(as.Date(start_date) , "%d%b%Y")
  end_date = format(as.Date(end_date) , "%d%b%Y")
  site_no = as.character(site_no)
  
  
  con <- paste0("&patientLoc=", {{state}}, "_")
  
  state_co <- tidycensus::fips_codes %>% filter(state == {{state}}) %>% 
    pull(county) %>% unlist %>% tolower %>% 
    gsub(" county", "", .) %>% 
    gsub("\\s", "%20", .) %>% 
    paste(collapse = con) %>% 
    paste0(con,.)
  
  
  clean_var_names <- purrr::compose(
    # remove extreme "_"
    function(x) gsub("^_|_$", "", x, perl = T), 
    # remove repeat "_"
    function(x) gsub("(_)(?=_*\\1)", "", x, perl = T), 
    # not [A-Za-z0-9_] and replace with "_"
    function(x) gsub("\\W", "_", x), 
    # parenthesis/bracket and its contents
    function(x) gsub("\\(.+\\)", "", x),
    function(x) gsub("\\[.+\\]", "", x),
    tolower)
  
  
  url <- glue::glue("https://essence.syndromicsurveillance.org/nssp_essence/api/tableBuilder/csv?endDate={end_date}&ccddCategory=cdc%20suicidal%20ideation%20v1&ccddCategory=cdc%20suicide%20attempt%20v1&ccddCategory=sdc%20suicide%20related%20v1&percentParam=ccddCategory&geographySystem=hospital&datasource=va_hosp&detector=nodetectordetector&startDate={start_date}&timeResolution={timeresolution}{state_co}&hasBeenE=1&medicalGroupingSystem=essencesyndromes&userId={user_id}&site={site_no}&hospFacilityType=emergency%20care&aqtTarget=TableBuilder&rowFields=timeResolution&rowFields=site&rowFields=patientLoc&columnField=ccddCategory")
  
  api_response <- GET(url, authenticate(user = username, password = password))
  
  co_snsro <- content(api_response, type = "text/csv")
  
  co_snsro <- co_snsro %>%
    set_names(clean_var_names) %>%
    select(site,
           patient_loc = patientloc,
           {{timeresolution}} := timeresolution,
           cdc_suicidal_ideation_v1_numerator=cdc_suicidal_ideation_v1_data_count,
           cdc_suicide_attempt_v1_numerator=cdc_suicide_attempt_v1_data_count,
           cdc_suicide_related_v1_numerator=sdc_suicide_related_v1_data_count,
           denominator=cdc_suicidal_ideation_v1_all_count)
  
  co_snsro
  
}
