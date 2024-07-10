
#############################################
# Data wrangling
# Script for cleaning the line list data
# Mark Otiende | Kennedy Mwai
#############################################

#'Scrap notes
#' rename and mutate mostly do it in the main data set
#' janitor package clean_names function can help you in cleaning variables
#' i.e hospital_df %>% clean_names() %>% glimpse
#' epikit package age_categories function creates age groups quickly


# Load packages -----------------------------------------------------------
library(rio)
library(epikit)
library(lubridate)
library(tidyverse)

# Question 1.1 --------------------------------------------------------------
## Import data -------------------------------------------------------------
hospital_df <- import("Data/line_hospital_data.csv")
location_df <- import("data/line_hospitals_locations.xlsx")


## check on the structure of the data --------------------------------------

glimpse(hospital_df)
glimpse(location_df)



# Question 1.2 ------------------------------------------------------------


## creating sub1 df --------------------------------------------------------
sub1 <- select(hospital_df, ct_blood, case_id, age)
hospital_df %>% 
  select(ct_blood,case_id,age)

glimpse(sub1)

## The data has 6481 rows and 3 columns


# Question 1.3 ------------------------------------------------------------

## rename variables with spaces --------------------------------------------
hospital_df <- rename(hospital_df,
                      #remember new name =  oldname
                      'date_onset' = 'date onset',
                      'hosp_date' = 'hosp date'
                      )


# Question 1.4 -------------------------------------------------------------
## Filtering participants who died -----------------------------------------
par_died <- hospital_df %>% 
  filter(outcome=="Death")
glimpse(par_died) ## Rows columns

## create a data set of males over 25 years -----------------------------------------

male_over25 <- hospital_df %>% 
  filter(age>25 & gender=="m")

## Use `filter` and `select` to show case_id , hosp_date and date_onset of patients who recovered. -----------------------------------------
## remember you have to filter using outcome since the variable used to filter is not in select 
### option not using pipe
par_recovered <- filter(hospital_df, outcome=="Recover")
par_recovered <- select(hospital_df ,case_id,hosp_date,date_onset)
### option using pipe
par_recovered <- hospital_df %>% 
  filter(outcome=="Recover") %>% 
  select(case_id,hosp_date,date_onset)

##  Create a data of participants who had `cough` and `chills` , then select only case_id,gender and age.

par_cough_chills <- hospital_df %>% 
  filter(cough=='yes' & chills=='yes') %>% 
  select(case_id,gender,age)


## Filter participants that had cough AND chills OR aches OR their ct_blood IS GREATER than 20 -------------------
par_pneumonia <- hospital_df %>% 
  filter((cough=='yes' & chills=='yes') | aches =='yes' | ct_blood>20) 
glimpse(par_pneumonia)


# Question 1.5 Mutate --------------------------------------------------------------

## Creating BMI -----------------------
hospital_df <- hospital_df %>% 
  mutate(ht_m=(ht_cm/100) ,
         BMI=wt_kg/(ht_m^2))
  
## Create a variable short is `YES` if ht_cm < 80 otherwise `NO` --------

hospital_df <- hospital_df %>% 
  mutate(short=ifelse(ht_cm<80,"YES","NO"))

##  Create age group of 10 year gaps, i.e `0-10,10-20,20-30,30-40,40-50,Over_50` ---------

hospital_df <- hospital_df %>% 
  mutate(age_group=case_when(
           age<10 ~"0-10",
           age >=10  & age< 20 ~"0-20", 
           age>=20 & age<30 ~ "20-30", 
           age>=30 & age<40 ~ "30-40", 
           age>=40 & age<50 ~ "40-50", 
           age>=50 ~ "Over 50", ## hapa ni ukweli --> check alaways the range
           
           .default = "other")) %>% 
  # create age category column
  mutate(age_cat = age_categories(        
    age,                             
    lower = 0,
    upper = 70,
    by = 10))


# Question 1.6 Merging -------------------------------------------------------------

## left join ----------------------
hosp_left_joined <- left_join(
  x = hospital_df,
  y = location_df,
  by = "case_id"
)

###' The above command returns a warning of many to many
###' most likely we have duplicates in case_id
###' check for duplicates hospital_df %>% get_dupes(case_id) using janitor package and it will add a column showing the count of dups
###'  remove duplicates using distinct function hospital_df %>% distinct(case_id ,.keep_all=T)

### -----------this keeps only the case_id with duplicates
dups_chek_a <- hospital_df %>% 
  select(case_id, hosp_date)  %>% 
  filter(duplicated(case_id ,hosp_date))

### -----------this keeps  case_id and gives you the count of duplicates
dups_check <- hospital_df %>% 
  select(case_id, hosp_date) %>% 
  janitor::get_dupes(case_id,hosp_date) ## always check your data to know the cause of duplicates

## inner join ----------------------
hosp_inner_joined <- inner_join(
  x = hospital_df,
  y = location_df,
  by = "case_id"
)

## full join ----------------------
hosp_full_joined <- full_join(
  x = hospital_df,
  y = location_df,
  by = "case_id"
)


glimpse(hosp_left_joined)
glimpse(hosp_inner_joined)
glimpse(hosp_full_joined)




# Question 1.8 Create a a cleaning pipe chain -----------------------------------------------------
hospital_df_adult <- hospital_df %>% 
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
  filter(age>18)

export(hospital_df_adult ,"data/hospital_adult_clean.csv") 



# Question 1.7 Extras --------------------------------------------------------------
## Working with dates
hospital_df <- hospital_df %>%
  ##converts to a date
  mutate(date_onset=as_date(date_onset)) %>% 
  ##extracts year and month
  mutate(hosp_year = year(date_onset) ,
         hosp_month = month(date_onset))

