get_ed_all_generic <- function(username, password, site_no, user_id, syndromic = "CDC All Drug v2", start_date, end_date, ...) {
  
  # replace the syndromic with the actual names as  listed below in comments
  # optional quoted selected fields: repleace "..." with any number of variables. 
  
  require(httr, quietly = T)
  require(glue, quietly = T)
  require(purrr, quietly = T)
  
  version <- as.character(version)
  start_date = gsub("^0", "", format(as.Date(start_date) , "%d%b%Y"))
  end_date = gsub("^0", "", format(as.Date(end_date) , "%d%b%Y"))
  syndromic = tolower(gsub("\\s", "%20" , syndromic, perl = T))
  
  url <- glue::glue("https://essence.syndromicsurveillance.org/nssp_essence/api/dataDetails/csv?endDate={end_date}&ccddCategory={syndromic}&percentParam=noPercent&geographySystem=hospital&datasource=va_hosp&detector=nodetectordetector&startDate={start_date}&ageNCHS=unknown&ageNCHS=00-10&ageNCHS=11-14&ageNCHS=15-24&ageNCHS=25-34&ageNCHS=35-44&ageNCHS=45-54&ageNCHS=55-64&ageNCHS=65-74&ageNCHS=75-84&ageNCHS=85-1000&timeResolution=daily&hasBeenE=1&medicalGroupingSystem=essencesyndromes&userId={user_id}&site={site_no}&hospFacilityType=emergency%20care&aqtTarget=DataDetails&refValues=true")
  
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
  
                                            # syndromic
  # 1                                CDC Asthma CCDD v1
  # 2               CDC AFM Broad v1-Limit to Pediatric
  # 3                                       ILI CCDD v1
  # 4                     Fever and Cough-Sob-DiffBr v1
  # 5    Fever and Cough-Sob-DiffBr neg Influenza DD v1
  # 6                      ILI Neg Influenza Mention v1
  # 7          CLI CC with CLI DD and Coronavirus DD v1
  # 8                     Fever and Cough-Sob-DiffBr v2
  # 9    Fever and Cough-Sob-DiffBr neg Influenza DD v2
  # 10         CLI CC with CLI DD and Coronavirus DD v2
  # 11               ILI Syndrome Neg Coronavirus DD v1
  # 12                   ILI CCDD Neg Coronavirus DD v1
  # 13                                     Marijuana v1
  # 14 CDC Pregnancy and Pregnancy Loss and Delivery v1
  # 15                                  Homelessness v1
  # 16                           SDC Suicide Related v1
  # 17                                   CDC Alcohol v1
  # 18                                  CDC All Drug v1
  # 19                           CDC Suicide Attempt v1
  # 20                                  CDC All Drug v2
  # 21                         CDC Suicidal Ideation v1
  # 22            SDC Disaster Related Mental Health v1
  # 23                           All Traffic Related v1
  # 24                           All Traffic Related v2
  # 25                            CDC Pneumonia CCDD v1
  # 26                                     Marijuana v2
  # 27                                     Marijuana v3
  # 28                               Sexual Violence v1
  # 29                               Sexual Violence v2
  # 30                           CDC Sexual Violence v3
  # 31             CDC AFM Narrow v1-Limit to Pediatric
  # 32                                  CDC Dialysis v1
  # 33                           CDC Opioid Overdose v1
  # 34                           CDC Opioid Overdose v2
  # 35                           CDC Opioid Overdose v3
  # 36                         CDC Medication Refill v1
  # 37                     CDC Diabetic Ketoacidosis v1
  # 38         CDC Suspected Child Abuse and Neglect v1
  # 39                            CDC Food Poisoning v1
  # 40                               CDC Hepatitis A v1
  # 41                       CDC Hand Foot and Mouth v1
  # 42                                CDC Stimulants v1
  # 43                                CDC Stimulants v2
  # 44                                CDC Stimulants v3
  # 45                          Heat Related Illness v1
  # 46                          Heat Related Illness v2
  # 47                               Visits of Interest
  # 48                          Cold Related Illness v1
  # 49                       CDC Chronic Hepatitis C v1
  # 50                                CDC Legionella v1
  # 51                             CDC Tick Exposure v1
  # 52                              CDC Measles CCDD v1
  # 53                               CDC Chicken Pox v1
  # 54                            CDC Firearm Injury v2
  # 55              CDC Unintentional Firearm Injury v1
  # 56                                   Foreign Travel
  # 57                                Foreign Travel v2
  # 58                     Intimate Partner Violence v1
  # 59                 CDC Intimate Partner Violence v2
  # 60                                     Norovirus v1
  # 61                              CDC Lyme Disease v1
  # 62                            CDC Firearm Injury V1
  # 63                              CDC Influenza DD v1
  # 64                           CDC Heroin Overdose v1
  # 65                           CDC Heroin Overdose v2
  # 66                           CDC Heroin Overdose v3
  # 67                           CDC Heroin Overdose v4
  # 68    CDC Unintentional Carbon Monoxide Exposure v1
  # 69         CDC Vaccine-Associated Adverse Events v1
  # 70                    CDC Assault Firearm Injury v1
  # 71                         CDC Acute Hepatitis C v1
  # 72                      Change in Taste or Smell v1
  # 73                                         Mumps v1
  # 74        CDC EVALI v1-Manually limit to ages 11-34
  # 75                            CDC Coronavirus-DD v1
  # 76                                 CDC Pertussis v1
  # 77                                  CDC Shigella v1
  # 78                    CDC Synthetic Cannabinoids v1
  # 79                CDC Intentional Firearm Injury v1
  # 80                 CDC Vaping and E Cig Injuries v1
  # 81                         CDC COVID-Specific DD v1
  # 
}

