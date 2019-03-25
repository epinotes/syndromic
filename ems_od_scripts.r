# Packages and functions --------------------------------------------------

SOAR::Attach()
library(tidyverse)
library(lubridate)
library(Hmisc)
library(injuryepi)
library(useicd10cm)

add_ems_icd <- function(data, diag_ecode_col) {
  anydrug_ <- "F1[1-9]|T3[6-9]|T4[0-9]|T50|Overdose"
  anyopioid_ <- "F11|T40\\.[0-46]|opioid"
  alcohol_ <- "F10|T51|alcohol"
  
  data %>% mutate(anydrug_icd = icd_new_diag(
    .,
    expr = anydrug_,
    colvec = diag_ecode_col
  ), anyopioid_icd = icd_new_diag(
    .,
    expr = anyopioid_,
    colvec = diag_ecode_col
  ), alcohol_icd = icd_new_diag(
    .,
    expr = alcohol_,
    colvec = diag_ecode_col
  ))
}

disposition_ <- c("Patient Dead at Scene-No Resuscitation Attempted (With Transport)",
                  "Patient Dead at Scene-No Resuscitation Attempted (Without Transport)",
                  "Patient Dead at Scene-Resuscitation Attempted (With Transport)",
                  "Patient Dead at Scene-Resuscitation Attempted (Without Transport)",
                  "Patient Evaluated, No Treatment/No Transport Required",
                  "Patient Refused Evaluation/Care (With Transport)",
                  "Patient Refused Evaluation/Care (Without Transport)",
                  "Patient Treated & Transported by Mutual Aid",
                  "Patient Treated, Released (AMA)",
                  "Patient Treated, Released (per protocol)",
                  "Patient Treated, Transferred Care to Another EMS Unit",
                  "Patient Treated, Transported by Another EMS Unit",
                  "Patient Treated, Transported by Law Enforcement",
                  "Patient Treated, Transported by Private Vehicle",
                  "Patient Treated, Transported by this EMS Unit",
                  "Patient Treated, Transported with this EMS Crew in Another Vehicle")



# Reading the data --------------------------------------------------------



ems_narcan_non_clinical <- read_csv("Narcan_Statewide_no_clinical.csv")

ems_narcan_non_clinical_list <- names(ems_narcan_non_clinical) %>% 
  enframe()

ems_narcan_non_clinical <- ems_narcan_non_clinical %>% 
  set_names(clean_var_names)

ems_narcan_clinical <- read_csv("Narcan_Statewide_clinical.csv")

ems_narcan_clinical_list <- names(ems_narcan_clinical) %>% 
  enframe()

ems_narcan_clinical <- ems_narcan_clinical %>% 
  set_names(clean_var_names)

sel_d <- setdiff(names(ems_narcan_clinical), names(ems_narcan_non_clinical))

sel_d <- c(sel_d, "fact_incident_pk")





## narrative

ems_narrative <- read_csv("Narcan_Statewide_narrative.csv")

ems_narrative_list <- names(ems_narrative) %>% 
  enframe()

ems_narrative <- ems_narrative %>% 
  set_names(clean_var_names)

sel_d2 <- c("fact_incident_pk", "patient_care_report_narrative")

ems_narcan <- ems_narcan_non_clinical %>% 
  left_join(ems_narcan_clinical[sel_d]) %>% 
  left_join(ems_narrative[sel_d2])

ems_narcan <- ems_narcan %>% 
  mutate(incident_county = str_to_title(incident_county),
         patient_home_county = str_to_title(patient_home_county))

ems_all <- read_csv("all_ems_query_date.csv")

ems_all_list <- names(ems_all) %>% 
  enframe()

ems_all <- ems_all %>% 
  set_names(clean_var_names)

ems_all <- ems_all %>% filter(disposition_incident_patient_disposition %in% disposition_)
#  nrow(ems_all) = 147715

ems_all <- ems_all %>% 
  mutate(scene_incident_county_name = str_to_title(scene_incident_county_name))

##
# 

# write_rds(ems_narcan_non_clinical, "Data/ems_narcan_non_clinical_20190313.rds")
# write_rds(ems_narcan_clinical, "Data/ems_narcan_clinical_20190313.rds") 
# write_rds(ems_narrative, "Data/ems_narrative_20190313.rds") 
# write_rds(ems_all, "Data/ems_all_20190313.rds") 

# Identifying  Narcan -----------------------------------------------------


narcan_ <- "narcan|nalox"

# describe(ems_narcan$medication)
# 
# describe(ems_narcan$fact_incident_pk)

ems_narcan <- ems_narcan %>%
  mutate(narcan = sign(grepl(narcan_, medication, ignore.case = T)))


## response to medication

# unique(ems_narcan$medication_response)

ems_narcan <- ems_narcan %>%
  mutate(response_to_med = sign(grepl("Improved", medication_response, ignore.case = T)))

ems_narcan <- ems_narcan %>%
  mutate(response_to_narcan = ifelse((narcan == 1 & response_to_med == 1), 1, 0 ))

## including narrative


ems_narcan <- ems_narcan %>% 
  mutate(narcan2 = icd_new_diag(.,expr = narcan_, colvec = c(10, 35)))

#ems_narcan %>% count(narcan2)

ems_narcan <- ems_narcan %>%
  mutate(response_to_narcan2 = ifelse((narcan2 == 1 & response_to_med == 1), 1, 0 ))


# clinical definition -----------------------------------------------------

## pinpoint pupils
ems_narcan <- ems_narcan %>% 
  mutate(pinpoint = sign(grepl("1-mm|2-mm|pinpoint", patient_eye_assessment_findings_list)))

## gcs under 14
ems_narcan <- ems_narcan %>% 
  mutate(gcs13 = ifelse(vitals_total_glasgow_coma_score_gcs < 14, 1, 0))

ems_narcan <- ems_narcan %>% 
  mutate(resp_rate12 = ifelse(vitals_respiratory_rate < 13, 1, 0))

ems_narcan <- ems_narcan %>% 
  mutate(gcs_resp = ifelse(gcs13 == 1 & resp_rate12 == 1, 1, 0))

ems_narcan <- ems_narcan %>% 
  mutate(gcs_resp_pin = ifelse(gcs13 == 1 & resp_rate12 == 1 & pinpoint == 1, 1, 0))


# Nesting and unnesting to resolve duplication ----------------------------


ems_narcan_nest <- ems_narcan %>%
  group_by(fact_incident_pk) %>%
  nest(.key = unique_case)


ems_narcan_nest1 <- ems_narcan_nest %>% 
  mutate(ems_nrow = map_dbl(unique_case, nrow))



# ems_narcan_nest1 %>% 
#   count(ems_nrow, sort = T)


ems_narcan_nest2 <- ems_narcan_nest1 %>% 
  mutate(ems_u = map(unique_case, f_comb2))


ems_narcan_unnest <- ems_narcan_nest2 %>% 
  unnest(ems_u) %>% 
  select(-unique_case)

## getting back the definitions without duplication

ems_narcan_unnest <- ems_narcan_unnest %>% 
  mutate(response_to_narcan = sign(grepl("1", response_to_narcan, ignore.case = T)))

ems_narcan_unnest <- ems_narcan_unnest %>% 
  mutate(narcan = sign(grepl("1", narcan, ignore.case = T)))

ems_narcan_unnest <- ems_narcan_unnest %>% 
  mutate(narcan2 = sign(grepl("1", narcan2, ignore.case = T)))

ems_narcan_unnest <- ems_narcan_unnest %>% 
  rename(narcan_nar = narcan2)


ems_narcan_unnest <- ems_narcan_unnest %>% 
  mutate(response_to_narcan2 = sign(grepl("1", response_to_narcan2, ignore.case = T)))

ems_narcan_unnest <- ems_narcan_unnest %>% 
  rename(response_to_narcan_nar = response_to_narcan2)






# injury and impression to define overdose

# ems_narcan_unnest %>% count(primary_impression, sort = T) %>% View()
# ems_narcan_unnest %>% count(secondary_impressions, sort = T) %>% View()
# ems_narcan_unnest %>% count(cause_of_injury, sort = T) %>% View()
# ems_narcan_unnest %>% count(cause_of_injury, sort = T) %>% View()



ems_narcan_unnest <- ems_narcan_unnest %>% 
  add_ems_icd(., c(22:24))


# ems_narcan_unnest %>% count(anyopioid_icd)
# 
# ems_narcan_unnest %>% count(response_to_narcan2, anyopioid_icd)

## Probable opioid for surveillance

ems_narcan_unnest <- ems_narcan_unnest %>%
  mutate(opioid_or_narcan_resp = ifelse((response_to_narcan == 1 | anyopioid_icd == 1), 1, 0 ))

ems_narcan_unnest <- ems_narcan_unnest %>%
  mutate(opioid_and_narcan_resp = ifelse((response_to_narcan == 1 & anyopioid_icd == 1), 1, 0 ))

# ems_narcan_unnest %>% count(opioid_and_narcan_resp)
# 
# ems_narcan_unnest %>% count(opioid_or_narcan_resp)

## clinical gcs resp

ems_narcan_unnest <- ems_narcan_unnest %>% 
  mutate(gcs_resp = sign(grepl("1", gcs_resp, ignore.case = T)))

## clinical gcs resp pinpoint

ems_narcan_unnest <- ems_narcan_unnest %>% 
  mutate(gcs_resp_pin = sign(grepl("1", gcs_resp_pin, ignore.case = T)))

## clinical  pinpoint

ems_narcan_unnest <- ems_narcan_unnest %>% 
  mutate(pinpoint = sign(grepl("1", pinpoint, ignore.case = T)))


## anydrug_icd" anyopioid_icd" alcohol_icd

# ems_narcan_unnest %>% count(anydrug_icd)
# ems_narcan_unnest %>% count(anyopioid_icd, narcan)
# ems_narcan_unnest %>% count(alcohol_icd)
# ems_narcan_unnest %>% count(medication_response, sort = T) %>% as.data.frame()

## adding months

ems_narcan_unnest <- ems_narcan_unnest %>% 
  mutate(date = mdy(incident_date),
         year_month = glue::glue("{year(date)}_{month(date, label = T)}"),
         year = year(date))

ems_narcan_unnest <- ems_narcan_unnest %>% 
  mutate(month = month(date, label = T))


# Building the data tables ------------------------------------------------




ems_narcan_unnest <- ems_narcan_unnest %>% 
  left_join(ach_ems_counties %>% 
              rename(incident_county = counties))

## county month tab

county_tab <- list(geography = c("Statewide", sort(unique(wa_counties_fips$county)), "out_of_state", "missing"), year_month = unique(ems_narcan_unnest$year_month)) %>% cross_df()

ems_narcan_unnest <- ems_narcan_unnest %>% 
  mutate(county = ifelse(incident_county %in% wa_counties_fips$county, incident_county, "out_of_state"))

ems_narcan_unnest <- ems_narcan_unnest %>% 
  mutate(county = ifelse(incident_county == "NA",  "missing", county)) 




## All ems

unique(ems_all$scene_incident_county_name)
ems_all <- ems_all %>%
  mutate(county = ifelse(scene_incident_county_name %in% wa_counties_fips$county, scene_incident_county_name, "out_of_state"))

ems_all <- ems_all %>% 
  mutate(county = ifelse(is.na(scene_incident_county_name),  "missing", county)) 


## adding months

ems_all <- ems_all %>% 
  mutate(date = mdy(incident_date),
         year_month = glue::glue("{year(date)}_{month(date, label = T)}"),
         year = year(date))

ems_all <- ems_all %>% 
  mutate(month = month(date, label = T))

ems_all_tab <- ems_all %>% 
  count(county, year_month) %>% 
  rename(all_ems = n,
         geography = county)

ems_all_tab_s <- ems_all %>% 
  count(year_month) %>% 
  rename(all_ems = n) %>% 
  add_column(geography = rep("Statewide", nrow(.)), .before = 1)


ems_all_tab_s <- ems_all %>% 
  count(county, year_month) %>% 
  rename(all_ems = n)

##

gather_sum <- function(.data,  ...) {
  # variable <- quo_name("variable")
  # value <- quo_name("value")
  .data %>%
    transmute(...) %>%
    gather("variable", "value" , 
           -one_of(group_vars(.))) %>%
    group_by(one_of(group_vars(.)), variable) %>%
    summarise_at(vars(value), sum) %>% 
    ungroup()
}

gather_grp <- function(.data,  ...) {
  # variable <- sym("variable")
  # value <- quo_name("value")
  .data %>%
    transmute(...) %>%
    gather("variable", "value" , 
           -one_of(group_vars(.))) %>%
    group_by(variable)
}

narc_tab_g <- ems_narcan_unnest %>% 
  select(county, year_month, narcan, gcs_resp) %>% 
  group_by(county, year_month) %>% 
  transmute(everything(.)) %>%
  gather("variable", "value" , 
         -one_of(group_vars(.))) 
gather_grp() %>%
  group_by(county,  year_month, variable) %>%
  summarise_at(vars(value), sum)

gather_sum(narcan, gcs_resp)



narc_tab <- ems_narcan_unnest %>% filter(narcan == 1) %>% 
  count(county, year_month) %>% 
  rename(geography = county) %>% na.omit()

narc_tab_s <- ems_narcan_unnest %>% filter(narcan == 1) %>% 
  count(year_month) %>% 
  add_column(geography = rep("Statewide", nrow(.)), .before = 1) 

narc_tab_a <- narc_tab_s %>% 
  bind_rows(narc_tab) %>% 
  right_join(county_tab) %>% 
  replace_na(list(n = 0)) %>% 
  rename(narcan = n)

##

narc2_tab <- ems_narcan_unnest %>% filter(narcan_nar == 1) %>% 
  count(county, year_month) %>% 
  rename(geography = county) %>% na.omit()

narc2_tab_s <- ems_narcan_unnest %>% filter(narcan_nar == 1) %>% 
  count(year_month) %>% 
  add_column(geography = rep("Statewide", nrow(.)), .before = 1) 

narc2_tab_a <- narc2_tab_s %>% 
  bind_rows(narc2_tab) %>% 
  right_join(county_tab) %>% 
  replace_na(list(n = 0)) %>% 
  rename(narcan_nar = n)


## 
ems_narcan_unnest <- ems_narcan_unnest %>% 
  rename(improved_to_narcan = response_to_narcan)

resp_narc_tab <- ems_narcan_unnest %>% filter(improved_to_narcan == 1) %>% 
  count(county, year_month) %>% 
  rename(geography = county) %>% na.omit()

resp_narc_tab_s <- ems_narcan_unnest %>% filter(improved_to_narcan == 1) %>% 
  count(year_month) %>% 
  add_column(geography = rep("Statewide", nrow(.)), .before = 1)

resp_narc_tab_a <- resp_narc_tab_s %>% 
  bind_rows(resp_narc_tab) %>% 
  right_join(county_tab) %>% 
  replace_na(list(n = 0)) %>% 
  rename(improved_to_narcan = n)

## 

ems_narcan_unnest <- ems_narcan_unnest %>% 
  rename(improved_to_narcan_nar = response_to_narcan_nar)

resp_narc2_tab <- ems_narcan_unnest %>% filter(improved_to_narcan_nar == 1) %>% 
  count(county, year_month) %>% 
  rename(geography = county) %>% na.omit()

resp_narc2_tab_s <- ems_narcan_unnest %>% filter(improved_to_narcan_nar == 1) %>% 
  count(year_month) %>% 
  add_column(geography = rep("Statewide", nrow(.)), .before = 1) 

resp_narc2_tab_a <- resp_narc2_tab_s %>% 
  bind_rows(resp_narc2_tab) %>% 
  right_join(county_tab) %>% 
  replace_na(list(n = 0)) %>% 
  rename(improved_to_narcan_nar = n)

##
ems_narcan_unnest <- ems_narcan_unnest %>% 
  rename(drug_icd = anydrug_icd)

drug_icd_tab <- ems_narcan_unnest %>% filter(drug_icd == 1) %>% 
  count(county, year_month) %>% 
  rename(geography = county) %>% na.omit()

drug_icd_tab_s <- ems_narcan_unnest %>% filter(drug_icd == 1) %>% 
  count(year_month) %>% 
  add_column(geography = rep("Statewide", nrow(.)), .before = 1) 

drug_icd_tab_a <- drug_icd_tab_s %>% 
  bind_rows(drug_icd_tab) %>% 
  right_join(county_tab) %>% 
  replace_na(list(n = 0)) %>% 
  rename(drug_icd = n)

## 

ems_narcan_unnest <- ems_narcan_unnest %>% 
  rename(opioid_icd = anyopioid_icd)

opi_icd_tab <- ems_narcan_unnest %>% filter(opioid_icd == 1) %>% 
  count(county, year_month) %>% 
  rename(geography = county) %>% na.omit()

opi_icd_tab_s <- ems_narcan_unnest %>% filter(opioid_icd == 1) %>% 
  count(year_month) %>% 
  add_column(geography = rep("Statewide", nrow(.)), .before = 1) 

opi_icd_tab_a <- opi_icd_tab_s %>% 
  bind_rows(opi_icd_tab) %>% 
  right_join(county_tab) %>% 
  replace_na(list(n = 0)) %>% 
  rename(opioid_icd = n)

## 
opi_and_narcan_resp_tab <- ems_narcan_unnest %>% filter(opioid_and_narcan_resp == 1) %>% 
  count(county, year_month) %>% 
  rename(geography = county) %>% na.omit()

opi_and_narcan_resp_tab_s <- ems_narcan_unnest %>% filter(opioid_and_narcan_resp == 1) %>% 
  count(year_month) %>% 
  add_column(geography = rep("Statewide", nrow(.)), .before = 1) 

opi_and_narcan_resp_tab_a <- opi_and_narcan_resp_tab_s %>% 
  bind_rows(opi_and_narcan_resp_tab) %>% 
  right_join(county_tab) %>% 
  replace_na(list(n = 0)) %>% 
  rename(opioid_and_narcan_resp = n) 

##

opi_or_narcan_resp_tab <- ems_narcan_unnest %>% filter(opioid_or_narcan_resp == 1) %>% 
  count(county, year_month) %>% 
  rename(geography = county) %>% na.omit()

opi_or_narcan_resp_tab_s <- ems_narcan_unnest %>% filter(opioid_or_narcan_resp == 1) %>% 
  count(year_month) %>% 
  add_column(geography = rep("Statewide", nrow(.)), .before = 1) 

opi_or_narcan_resp_tab_a <- opi_or_narcan_resp_tab_s %>% 
  bind_rows(opi_or_narcan_resp_tab) %>% 
  right_join(county_tab) %>% 
  replace_na(list(n = 0)) %>% 
  rename(opioid_or_narcan_resp = n) 

##

##

gcs_resp_tab <- ems_narcan_unnest %>% filter(gcs_resp == 1) %>% 
  count(county, year_month) %>% 
  rename(geography = county) %>% na.omit()

gcs_resp_tab_s <- ems_narcan_unnest %>% filter(gcs_resp == 1) %>% 
  count(year_month) %>% 
  add_column(geography = rep("Statewide", nrow(.)), .before = 1) 

gcs_resp_tab_a <- gcs_resp_tab_s %>% 
  bind_rows(gcs_resp_tab) %>% 
  right_join(county_tab) %>% 
  replace_na(list(n = 0)) %>% 
  rename(gcs_resp = n) 

## final table

ems_opi_tab <- narc_tab_a %>% 
  left_join(resp_narc_tab_a) %>% 
  left_join(opi_icd_tab_a) %>% 
  left_join(opi_or_narcan_resp_tab_a) %>% 
  left_join(opi_and_narcan_resp_tab_a) %>%
  left_join(drug_icd_tab_a) %>%
  left_join(gcs_resp_tab_a) %>%
  left_join(narc2_tab_a) %>%
  left_join(resp_narc2_tab_a) 

## add ems and ach  

ach_ems <- ach_ems_counties %>%
  rename(geography = counties) %>% 
  add_row(ACH= "All", ach_short = "All", geography = "Statewide", ems_region = "All", .before = 1)

ems_opi_tab <- ems_opi_tab %>% 
  left_join(ach_ems)




## ems regions

ems_opi_tab_reg <- ems_opi_tab %>% 
  filter(geography != "Statewide") %>% 
  left_join(ems_regions %>% select(-id) %>% 
              rename(geography = county))

ems_opi_tab_reg <- ems_opi_tab %>% 
  group_by(ems_region, year_month) %>% 
  summarise_at(vars(narcan, improved_to_narcan, opioid_icd, opioid_or_narcan_resp, opioid_and_narcan_resp, drug_icd, gcs_resp, narcan_nar, improved_to_narcan_nar), sum)

## ach 

ems_opi_tab_ach <- ems_opi_tab %>% 
  group_by(ACH, year_month) %>% 
  summarise_at(vars(narcan, improved_to_narcan, opioid_icd, opioid_or_narcan_resp, opioid_and_narcan_resp, drug_icd, gcs_resp, narcan_nar, improved_to_narcan_nar), sum)


# agency

ems_opi_tab_agency <- ems_narcan_unnest %>% 
  filter(opioid_or_narcan_resp == 1) %>% 
  count(county, agency) %>% arrange(county) %>% 
  rename(opioid_or_narcan_resp = n)



