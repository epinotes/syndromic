
# Needed packages

library(tidyverse)
# install.packages("remotes")
# remotes::install_github("epinotes/useicd10cm")
library(useicd10cm)


## functions  to identify any drug,  any opioid or alcohol

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

###

# Disposition Incident Patient Disposition (eDisposition.12)


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


## non-clinical

ems_narcan_non_clinical <- read_csv("monthly/Narcan_Statewide_no_clinical_2019-02-19_013930.csv")

ems_narcan_non_clinical_list <- names(ems_narcan_non_clinical) %>% 
  enframe()

ems_narcan_non_clinical <- ems_narcan_non_clinical %>% 
  set_names(clean_var_names)

# clinical data

ems_narcan_clinical <- read_csv("monthly/Narcan_Statewide_clinical_2019-02-19_015854.csv")

ems_narcan_clinical_list <- names(ems_narcan_clinical) %>% 
  enframe()

ems_narcan_clinical <- ems_narcan_clinical %>% 
  set_names(clean_var_names)

sel_d <- setdiff(names(ems_narcan_clinical), names(ems_narcan_non_clinical))

sel_d <- c(sel_d, "fact_incident_pk")

ems_narcan <- ems_narcan_non_clinical %>% 
  left_join(ems_narcan_clinical[sel_d])

## narrative data

ems_narrative <- read_csv("monthly/Narcan_Statewide_narrative_2019-02-19_023140.csv")

ems_narrative_list <- names(ems_narrative) %>% 
  enframe()

ems_narrative <- ems_narrative %>% 
  set_names(clean_var_names)

sel_d2 <- c("fact_incident_pk", "patient_care_report_narrative")

ems_narcan <- ems_narcan %>% 
  left_join(ems_narrative[sel_d2])

# all ems 

ems_all <- read_csv("monthly/all_ems_query_date_2019-02-25_194400.csv")

ems_all_list <- names(ems_all) %>% 
  enframe()

ems_all <- ems_all %>% 
  set_names(clean_var_names)




# Identifying  Narcan -----------------------------------------------------


narcan_ <- "narcan|nalox"
describe(ems_narcan$medication)

ems_narcan <- ems_narcan %>%
  mutate(narcan = sign(grepl(narcan_, medication, ignore.case = T)))


## response to medication

unique(ems_narcan$medication_response)

ems_narcan <- ems_narcan %>%
  mutate(response_to_med = sign(grepl("Improved", medication_response, ignore.case = T)))

ems_narcan <- ems_narcan %>%
  mutate(response_to_narcan = ifelse((narcan ==1 & response_to_med == 1), 1, 0 ))

## including narrative


ems_narcan <- ems_narcan %>% 
  mutate(narcan2 = icd_new_diag(.,expr = narcan_, colvec = c(10, 35)))

ems_narcan <- ems_narcan %>%
  mutate(response_to_narcan2 = ifelse((narcan2 ==1 & response_to_med == 1), 1, 0 ))


# Nesting and unnesting to resolve duplication ----------------------------


ems_narcan_nest <- ems_narcan %>%
  group_by(fact_incident_pk) %>%
  nest(.key = unique_case)


ems_narcan_nest1 <- ems_narcan_nest %>% 
  mutate(ems_nrow = map_dbl(unique_case, nrow))

ems_narcan_nest1 %>% 
  count(ems_nrow, sort = T)


ems_narcan_nest2 <- ems_narcan_nest1 %>% 
  mutate(ems_u = map(unique_case, f_comb2))


ems_narcan_unnest <- ems_narcan_nest2 %>% 
  unnest(ems_u) %>% 
  select(-unique_case)

## getting back the codes for narcan without narrative or with narrative

ems_narcan_unnest <- ems_narcan_unnest %>% 
  mutate(response_to_narcan = sign(grepl("1", response_to_narcan, ignore.case = T)))

ems_narcan_unnest <- ems_narcan_unnest %>% 
  mutate(narcan = sign(grepl("1", narcan, ignore.case = T)))

ems_narcan_unnest <- ems_narcan_unnest %>% 
  mutate(narcan2 = sign(grepl("1", narcan2, ignore.case = T)))

ems_narcan_unnest <- ems_narcan_unnest %>% 
  mutate(response_to_narcan2 = sign(grepl("1", response_to_narcan2, ignore.case = T)))






# Define overdoses from injury and impression


ems_narcan_unnest <- ems_narcan_unnest %>% 
  add_ems_icd(., c(22:24))


ems_narcan_unnest %>% count(anyopioid_icd)

ems_narcan_unnest %>% count(response_to_narcan2, anyopioid_icd)

## Probable opioid for surveillance based on response to narcan and anyopioid as defined above

ems_narcan_unnest <- ems_narcan_unnest %>%
  mutate(opioid_narcan_resp = ifelse((response_to_narcan == 1 | anyopioid_icd == 1), 1, 0 ))

ems_narcan_unnest <- ems_narcan_unnest %>%
  mutate(opioid_prob = ifelse((response_to_narcan2 ==1 | anyopioid_icd == 1), 1, 0 ))

ems_narcan_unnest %>% count(opioid_prob)

ems_narcan_unnest %>% count(opioid_narcan_resp)


## adding months

ems_narcan_unnest <- ems_narcan_unnest %>% 
  mutate(date = mdy(incident_date),
         year_month = glue::glue("{year(date)}_{month(date, label = T)}"),
         year = year(date))

ems_narcan_unnest <- ems_narcan_unnest %>% 
  mutate(month = month(date, label = T))

ems_narcan_unnest %>% count(year_month)



ems_narcan_unnest %>% filter(narcan == 1) %>% 
  count(incident_county, sort = T)


# Building the data tables ------------------------------------------------

## county month tab

county_tab <- list(geography = c("Statewide", sort(unique(wa_counties_fips$county))), year_month = unique(ems_narcan_unnest$year_month)) %>% cross_df()

ems_narcan_unnest <- ems_narcan_unnest %>% 
  mutate(county = ifelse(!(incident_county %in% wa_counties_fips$county), NA, incident_county))

##

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

narc2_tab <- ems_narcan_unnest %>% filter(narcan2 == 1) %>% 
  count(county, year_month) %>% 
  rename(geography = county) %>% na.omit()

narc2_tab_s <- ems_narcan_unnest %>% filter(narcan2 == 1) %>% 
  count(year_month) %>% 
  add_column(geography = rep("Statewide", nrow(.)), .before = 1) 

narc2_tab_a <- narc2_tab_s %>% 
  bind_rows(narc2_tab) %>% 
  right_join(county_tab) %>% 
  replace_na(list(n = 0)) %>% 
  rename(narcan2 = n)


## 
resp_narc_tab <- ems_narcan_unnest %>% filter(response_to_narcan == 1) %>% 
  count(county, year_month) %>% 
  rename(geography = county) %>% na.omit()

resp_narc_tab_s <- ems_narcan_unnest %>% filter(response_to_narcan == 1) %>% 
  count(year_month) %>% 
  add_column(geography = rep("Statewide", nrow(.)), .before = 1)

resp_narc_tab_a <- resp_narc_tab_s %>% 
  bind_rows(resp_narc_tab) %>% 
  right_join(county_tab) %>% 
  replace_na(list(n = 0)) %>% 
  rename(improved_to_narcan = n)

## 
resp_narc2_tab <- ems_narcan_unnest %>% filter(response_to_narcan2 == 1) %>% 
  count(county, year_month) %>% 
  rename(geography = county) %>% na.omit()

resp_narc2_tab_s <- ems_narcan_unnest %>% filter(response_to_narcan2 == 1) %>% 
  count(year_month) %>% 
  add_column(geography = rep("Statewide", nrow(.)), .before = 1) 

resp_narc2_tab_a <- resp_narc2_tab_s %>% 
  bind_rows(resp_narc2_tab) %>% 
  right_join(county_tab) %>% 
  replace_na(list(n = 0)) %>% 
  rename(improved_to_narcan2 = n)

##

drug_icd_tab <- ems_narcan_unnest %>% filter(anydrug_icd == 1) %>% 
  count(county, year_month) %>% 
  rename(geography = county) %>% na.omit()

drug_icd_tab_s <- ems_narcan_unnest %>% filter(anydrug_icd == 1) %>% 
  count(year_month) %>% 
  add_column(geography = rep("Statewide", nrow(.)), .before = 1) 

drug_icd_tab_a <- drug_icd_tab_s %>% 
  bind_rows(drug_icd_tab) %>% 
  right_join(county_tab) %>% 
  replace_na(list(n = 0)) %>% 
  rename(drug_icd = n)

## 
opi_icd_tab <- ems_narcan_unnest %>% filter(anyopioid_icd == 1) %>% 
  count(county, year_month) %>% 
  rename(geography = county) %>% na.omit()

opi_icd_tab_s <- ems_narcan_unnest %>% filter(anyopioid_icd == 1) %>% 
  count(year_month) %>% 
  add_column(geography = rep("Statewide", nrow(.)), .before = 1) 

opi_icd_tab_a <- opi_icd_tab_s %>% 
  bind_rows(opi_icd_tab) %>% 
  right_join(county_tab) %>% 
  replace_na(list(n = 0)) %>% 
  rename(opioid_icd = n)

## 
opi_prob_tab <- ems_narcan_unnest %>% filter(opioid_prob == 1) %>% 
  count(county, year_month) %>% 
  rename(geography = county) %>% na.omit()

opi_prob_tab_s <- ems_narcan_unnest %>% filter(opioid_prob == 1) %>% 
  count(year_month) %>% 
  add_column(geography = rep("Statewide", nrow(.)), .before = 1) 

opi_narc_resp_tab <- opi_prob_tab_s %>% 
  bind_rows(opi_prob_tab) %>% 
  right_join(county_tab) %>% 
  replace_na(list(n = 0)) %>% 
  rename(probable_opioid = n) 

##

opi_narc_resp_tab <- ems_narcan_unnest %>% filter(opioid_narcan_resp == 1) %>% 
  count(county, year_month) %>% 
  rename(geography = county) %>% na.omit()

opi_narc_resp_tab_s <- ems_narcan_unnest %>% filter(opioid_narcan_resp == 1) %>% 
  count(year_month) %>% 
  add_column(geography = rep("Statewide", nrow(.)), .before = 1) 

opi_narc_resp_tab_a <- opi_narc_resp_tab_s %>% 
  bind_rows(opi_narc_resp_tab) %>% 
  right_join(county_tab) %>% 
  replace_na(list(n = 0)) %>% 
  rename(opioid_narcan_resp = n) 

## final table

ems_opi_tab <- narc_tab_a %>% 
  left_join(narc2_tab_a) %>% 
  left_join(resp_narc_tab_a) %>% 
  left_join(resp_narc2_tab_a) %>% 
  left_join(opi_narc_resp_tab_a) %>%
  left_join(opi_prob_tab_a) %>% 
  left_join(opi_icd_tab_a) %>% 
  left_join(drug_icd_tab_a) 
