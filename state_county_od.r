#' Title
#'
#' @param username 
#' @param password 
#' @param site_no 
#' @param user_id 
#' @param state 
#' @param start_date 
#' @param end_date 
#'
#' @return
#' @export
#'
#' @examples
state_county_od <- state_county_od <- function(username, password, site_no, user_id, state, start_date, end_date) {
  
  # dates argument in the format "2019-01-31"
  # state argument in the format of quoted two character state name example state = "WA
  
  require(dplyr, quietly = T)
  require(httr, quietly = T)
  require(glue, quietly = T)
  require(purrr, quietly = T)
  require(janitor, quietly = T)
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
  
  clean_var_names <- purrr:compose(
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
  
  
  url <- glue::glue("https://essence.syndromicsurveillance.org/nssp_essence/api/tableBuilder/csv?endDate={end_date}&ccddCategory=cdc%20stimulants%20v3&ccddCategory=cdc%20opioid%20overdose%20v2&ccddCategory=cdc%20heroin%20overdose%20v4&ccddCategory=cdc%20all%20drug%20v1&percentParam=ccddCategory&geographySystem=hospital&datasource=va_hosp&detector=nodetectordetector&startDate={start_date}&timeResolution=monthly{state_co}&hasBeenE=1&medicalGroupingSystem=essencesyndromes&userId={user_id}&site={site_no}&hospFacilityType=emergency%20care&aqtTarget=TableBuilder&rowFields=timeResolution&rowFields=site&rowFields=patientLoc&columnField=ccddCategory")
  
  api_response <- GET(url, authenticate(user = username, password = password))
  
  co_od <- content(api_response, type = "text/csv")
  
  co_od <- co_od %>%
    set_names(clean_var_names) %>%
    select(site,
           patient_loc,
           year_month = time_resolution,
           cdc_all_drug_v1_numerator=cdc_all_drug_v1_data_count,
           cdc_opioid_overdose_v2_numerator=cdc_opioid_overdose_v2_data_count,
           cdc_heroin_overdose_v4_numerator=cdc_heroin_overdose_v4_data_count,
           cdc_stimulants_v3_numerator=cdc_stimulants_v3_data_count,
           denominator=cdc_opioid_overdose_v2_all_count) %>%
    separate(year_month, c("Year", "Month"))
  
  co_od
  
}