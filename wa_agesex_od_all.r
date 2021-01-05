wa_agesex_od_all <- function(username, password, 
                         site_no, user_id, 
                         start_date, end_date,
                            site) {
 require(httr, quietly = T)
  require(glue, quietly = T)
  require(purrr, quietly = T)
  require(dplyr, quietly = T)
  require(tidyr, quietly = T)
  
  start_date = format(as.Date(start_date) , "%d%b%Y")
  end_date = format(as.Date(end_date) , "%d%b%Y")
  site_no = as.character(site_no)
  
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
  

  url <- glue::glue("https://essence.syndromicsurveillance.org/nssp_essence/api/tableBuilder/csv?endDate={end_date}&percentParam=noPercent&datasource=va_hosp&startDate={start_date}&medicalGroupingSystem=essencesyndromes&userId={user_id}&site={site_no}&hospFacilityType=emergency%20care&aqtTarget=TableBuilder&ccddCategory=cdc%20stimulants%20v3&ccddCategory=cdc%20opioid%20overdose%20v3&ccddCategory=cdc%20heroin%20overdose%20v4&ccddCategory=cdc%20all%20drug%20v2&geographySystem=hospital&detector=nodetectordetector&timeResolution=monthly&hasBeenE=1&rowFields=sex&rowFields=ageNCHS&rowFields=timeResolution&columnField=ccddCategory")
  
  url2 <- glue::glue("https://essence.syndromicsurveillance.org/nssp_essence/api/tableBuilder/csv?endDate={end_date}&percentParam=noPercent&datasource=va_hosp&startDate={start_date}&medicalGroupingSystem=essencesyndromes&userId={user_id}&site={site_no}&hospFacilityType=emergency%20care&aqtTarget=TableBuilder&geographySystem=hospital&detector=nodetectordetector&timeResolution=monthly&hasBeenE=1&rowFields=sex&rowFields=ageNCHS&columnField=timeResolution")
  
  api_response <- httr::GET(url, httr::authenticate(user = username, password = password))

  result_site_ageSex <- content(api_response, type = "text/csv") %>%
    set_names(clean_var_names)

 
  api_response2 <- httr::GET(url2, httr::authenticate(user = username, password = password))
  
  result_site_total <- content(api_response2, type = "text/csv") %>%
    set_names(clean_var_names) %>%
    pivot_longer(cols = -c(sex, agenchs),
                 names_to = "timeresolution",
                 values_to = "denominator") %>% 
    mutate(timeresolution = gsub("_", "-", timeresolution))
  
  result_site_ageSex <- result_site_ageSex %>% 
    left_join(result_site_total,
              by = c("sex" = "sex", "agenchs" = "agenchs", "timeresolution" = "timeresolution"))
  
  result_site_ageSex <- result_site_ageSex %>%
    mutate(site = site) %>% 
    select(
      site, 
      year_month = timeresolution,
      sex,
      age_nchs = agenchs,
      cdc_all_drug_v2_numerator=cdc_all_drug_v2,
      cdc_opioid_overdose_v3_numerator=cdc_opioid_overdose_v3,
      cdc_heroin_overdose_v4_numerator=cdc_heroin_overdose_v4,
      cdc_stimulants_v3_numerator=cdc_stimulants_v3,
      denominator)  
  
  resultM_F <- result_site_ageSex %>%
    filter(sex %in% c("Male", "Female")) 
  
  resultmissing <- result_site_ageSex %>%
    filter(sex %in% c("Not Reported", "Unknown")) %>%
    group_by(site, year_month, age_nchs) %>%
    summarise_at(c("cdc_all_drug_v2_numerator",
                   "cdc_opioid_overdose_v3_numerator",
                   "cdc_heroin_overdose_v4_numerator",
                   "cdc_stimulants_v3_numerator",
                   "denominator"), sum) %>%
    mutate(sex = "Missing")
  
  result_site_ageSex <- bind_rows(resultM_F, resultmissing)
  
  result_site_ageSex %>%
    separate(year_month, c("Year", "Month")) %>% 
    group_by(site,  Year,  Month, sex,   age_nchs) %>% 
    summarise(across(c(cdc_all_drug_v2_numerator, cdc_opioid_overdose_v3_numerator, cdc_heroin_overdose_v4_numerator, cdc_stimulants_v3_numerator, denominator), sum), .groups = "drop") 
}

