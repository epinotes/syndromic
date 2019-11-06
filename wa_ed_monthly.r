wa_ed_monthly <- function(username = kr_username, password = kr_password, 
                           site_no = kr_siteno, user_id = kr_userid, 
                           start_date, end_date, ...) {
  require(httr, quietly = T)
  require(glue, quietly = T)
  require(purrr, quietly = T)
  require(janitor, quietly = T)
  
  start_date = format(as.Date(start_date) , "%d%b%Y")
  end_date = format(as.Date(end_date) , "%d%b%Y")
  site_no = as.character(site_no)
  
  
  url <- glue::glue("https://essence.syndromicsurveillance.org/nssp_essence/api/dataDetails/csv?endDate={end_date}&timeResolution=monthly&percentParam=ccddCategory&geographySystem=hospital&datasource=va_hosp&detector=nodetectordetector&startDate={start_date}&patientLoc=wa_adams&patientLoc=wa_asotin&patientLoc=wa_benton&patientLoc=wa_chelan&patientLoc=wa_clallam&patientLoc=wa_clark&patientLoc=wa_columbia&patientLoc=wa_cowlitz&patientLoc=wa_douglas&patientLoc=wa_ferry&patientLoc=wa_franklin&patientLoc=wa_garfield&patientLoc=wa_grant&patientLoc=wa_grays%20harbor&patientLoc=wa_island&patientLoc=wa_jefferson&patientLoc=wa_king&patientLoc=wa_kitsap&patientLoc=wa_kittitas&patientLoc=wa_klickitat&patientLoc=wa_lewis&patientLoc=wa_lincoln&patientLoc=wa_mason&patientLoc=wa_okanogan&patientLoc=wa_pacific&patientLoc=wa_pend%20oreille&patientLoc=wa_pierce&patientLoc=wa_san%20juan&patientLoc=wa_skagit&patientLoc=wa_skamania&patientLoc=wa_snohomish&patientLoc=wa_spokane&patientLoc=wa_stevens&patientLoc=wa_thurston&patientLoc=wa_wahkiakum&patientLoc=wa_walla%20walla&patientLoc=wa_whatcom&patientLoc=wa_whitman&patientLoc=wa_yakima&hasBeenE=1&medicalGroupingSystem=essencesyndromes&userId={user_id}&site={site_no}&hospFacilityType=emergency%20care&userId={user_id}&site={site_no}&hospFacilityType=emergency%20care&aqtTarget=DataDetails&refValues=true")
  
  f_field <- function(url, ...){
    
    if(!length(list(...))){
      field <- NULL
    } 
    else { 
      field <- paste(paste0("&field=",list(...)), collapse="")   
    }
    paste0(url, field)
  }
  
  url <- f_field(url = url, ...)
  api_resp <- GET(url, authenticate(user = username, password = password))
  content(api_resp, type = "text/csv")
  
}

# example of use

# wa_ed_sep19 <- wa_ed_monthly(username = kr_username, password = kr_password, 
#                               site_no = kr_siteno, user_id = kr_userid,
#                               start_date = "2019-09-01", end_date = "2019-09-30",
#                              "C_Processed_BioSense_ID", "ChiefComplaintParsed",
#                              "CCAvailable", "CCInformative",
#                              "DischargeDiagnosis", "DischargeDiagnosisUpdates",
#                              "DDAvailable", "DDInformative")

