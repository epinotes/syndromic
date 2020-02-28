wa_agesex_od <- function(username, password, 
                           site_no, user_id, 
                           start_date, end_date) {
  require(httr, quietly = T)
  require(glue, quietly = T)
  require(purrr, quietly = T)
  
  start_date = format(as.Date(start_date) , "%d%b%Y")
  end_date = format(as.Date(end_date) , "%d%b%Y")
  site_no = as.character(site_no)
  
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


  
  url <- glue::glue("https://essence.syndromicsurveillance.org/nssp_essence/api/tableBuilder/csv?endDate={end_date}&ccddCategory=cdc%20stimulants%20v3&ccddCategory=cdc%20opioid%20overdose%20v2&ccddCategory=cdc%20heroin%20overdose%20v4&ccddCategory=cdc%20all%20drug%20v1&percentParam=ccddCategory&geographySystem=hospital&datasource=va_hosp&detector=nodetectordetector&startDate={start_date}&timeResolution=monthly&patientLoc=wa_adams&patientLoc=wa_asotin&patientLoc=wa_benton&patientLoc=wa_chelan&patientLoc=wa_clallam&patientLoc=wa_clark&patientLoc=wa_columbia&patientLoc=wa_cowlitz&patientLoc=wa_douglas&patientLoc=wa_ferry&patientLoc=wa_franklin&patientLoc=wa_garfield&patientLoc=wa_grant&patientLoc=wa_grays%20harbor&patientLoc=wa_island&patientLoc=wa_jefferson&patientLoc=wa_king&patientLoc=wa_kitsap&patientLoc=wa_kittitas&patientLoc=wa_klickitat&patientLoc=wa_lewis&patientLoc=wa_lincoln&patientLoc=wa_mason&patientLoc=wa_okanogan&patientLoc=wa_pacific&patientLoc=wa_pend%20oreille&patientLoc=wa_pierce&patientLoc=wa_san%20juan&patientLoc=wa_skagit&patientLoc=wa_skamania&patientLoc=wa_snohomish&patientLoc=wa_spokane&patientLoc=wa_stevens&patientLoc=wa_thurston&patientLoc=wa_wahkiakum&patientLoc=wa_walla%20walla&patientLoc=wa_whatcom&patientLoc=wa_whitman&patientLoc=wa_yakima&hasBeenE=1&medicalGroupingSystem=essencesyndromes&userId={user_id}&site={site_no}&hospFacilityType=emergency%20care&aqtTarget=TableBuilder&rowFields=timeResolution&rowFields=site&rowFields=patientLoc&rowFields=sex&rowFields=ageNCHS&columnField=ccddCategory")
  
  api_response <- GET(url, authenticate(user = username, password = password))
  
  
  result_site_ageSex <- content(api_response, type = "text/csv") %>%
    set_names(clean_var_names) %>%
    select(site,
           year_month = time_resolution,
           sex,
           age_nchs,
           cdc_all_drug_v1_numerator=cdc_all_drug_v1_data_count,
           cdc_opioid_overdose_v2_numerator=cdc_opioid_overdose_v2_data_count,
           cdc_heroin_overdose_v4_numerator=cdc_heroin_overdose_v4_data_count,
           cdc_stimulants_v3_numerator=cdc_stimulants_v3_data_count,
           denominator=cdc_opioid_overdose_v2_all_count)
  
  resultM_F <- result_site_ageSex %>%
    filter(sex %in% c("Male", "Female")) 
  
  resultmissing <- result_site_ageSex %>%
    filter(sex %in% c("Not Reported", "Unknown")) %>%
    group_by(site, year_month, age_nchs) %>%
    summarise_at(c("cdc_all_drug_v1_numerator",
                   "cdc_opioid_overdose_v2_numerator",
                   "cdc_heroin_overdose_v4_numerator",
                   "cdc_stimulants_v3_numerator",
                   "denominator"), sum) %>%
    mutate(sex = "Missing")
  
  result_site_ageSex <- bind_rows(resultM_F, resultmissing)
  
  result_site_ageSex %>%
    separate(year_month, c("Year", "Month")) %>% 
    group_by(site,  Year,  Month, sex,   age_nchs) %>% 
    summarise_at(vars(cdc_all_drug_v1_numerator, cdc_opioid_overdose_v2_numerator, cdc_heroin_overdose_v4_numerator, cdc_stimulants_v3_numerator, denominator), sum) %>% 
    ungroup
  
}
