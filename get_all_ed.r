#' Title
#'
#' @param username 
#' @param password 
#' @param site_no 
#' @param user_id 
#' @param start_date 
#' @param end_date 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
get_all_ed <- function(username, password,
                       site_no, user_id,
                       start_date, end_date, ...) {
  require(httr, quietly = T)
  require(glue, quietly = T)
  require(purrr, quietly = T)

  start_date = format(as.Date(start_date) , "%d%b%Y")
  end_date = format(as.Date(end_date) , "%d%b%Y")
  site_no = as.character(site_no)

  url <- glue::glue("https://essence.syndromicsurveillance.org/nssp_essence/api/dataDetails/csv?endDate={end_date}&percentParam=noPercent&geographySystem=hospital&datasource=va_hosp&detector=nodetectordetector&startDate={start_date}&ageNCHS=unknown&ageNCHS=00-10&ageNCHS=11-14&ageNCHS=15-24&ageNCHS=25-34&ageNCHS=35-44&ageNCHS=45-54&ageNCHS=55-64&ageNCHS=65-74&ageNCHS=75-84&ageNCHS=85-1000&timeResolution=daily&hasBeenE=1&medicalGroupingSystem=essencesyndromes&userId={user_id}&site={site_no}&hospFacilityType=emergency%20care&aqtTarget=DataDetails&refValues=true")


  ## select fields

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
