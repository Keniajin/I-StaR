
#############################################
# Data wrangling
# Script for cleaning the line list data
#############################################



# Load packages -----------------------------------------------------------
library(rio)
library(epikit)
library(janitor)
library(lubridate)
library(tidyverse)

hospital_df <- import("Data/line_hospital_data.csv")
location_df <- import("data/line_hospitals_locations.xlsx")

# Question 1.2CCreate a pipe chain to clean the hospital_df-----------------------------------------------------
hospital_df_clean <- hospital_df %>% 
  ## this function from janitor helps remove spaces and non standard names
  clean_names() %>%  
  ##remove duplicates to avoid warnings during merge
  distinct(case_id , .keep_all = T) %>% 
  ## creating BMI
  mutate(ht_m = ht_cm/100 ,
         BMI = wt_kg/(ht_m^2)) %>% 
  # create age category column
  mutate(age_cat = age_categories(        
    age,                             
    lower = 0,
    upper = 70,
    by = 10)) %>% 
  ##convert dates
  mutate(date_of_outcome=dmy(date_of_outcome),
         hosp_date=dmy(hosp_date),
         date_onset=as_date(date_onset)
         ) %>% 
  #creat year and month
  mutate(year_hosp=year(hosp_date),
         month_hosp=month(hosp_date),
         
         year_onset=year(date_onset),
         #Create a `year_onset` and `month_onset` from date onset date data
         month_onset=month(date_onset) ,
         #Create a new variable `days_to_hosp`by subtracting `hosp_date` - `date_onset`
         days_to_hosp=hosp_date - date_onset)


# 1.3 Merge the datasets --------------------------------------------------

hospital_df_merged <- hospital_df_clean %>% 
  left_join(location_df , by = 'case_id')


## clean hospital
hospital_df_merged <- hospital_df_merged %>% 
  # re-code hospital column to have same ne
  mutate(hospital = recode(hospital,
                           # for reference: OLD = NEW
                           "Mitilary Hospital"  = "Military Hospital",
                           "Port"               = "Port Hospital",
                           "Port Hopital"       = "Port Hospital",
                           'Military Hopital' = "Military Hospital",
                           "Mitylira Hopital"   = "Military Hospital",
                           "Mitylira Hospital"  = "Military Hospital",
                           'Central Hopital'   = 'Central Hospital',
                           'St. Marks Maternity Hopital (SMMH)' = "SMMH",
                           "St. Mark's Maternity Hospital (SMMH)" = "SMMH"))
