
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

hospital_df <- import("data/line_hospital_data.csv")
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



# 1.4 Plot Histogram and density plots--------------------------------------------------

#Plot a histogram and density of the age variable
ggplot(hospital_df_merged,aes(age)) +
  geom_histogram(bins = 5)

ggplot(hospital_df_merged,aes(age)) +
  geom_density()

#Plot a histogram and density of the weight variable
ggplot(hospital_df_merged,aes(wt_kg)) +
  geom_histogram()

ggplot(hospital_df_merged,aes(wt_kg)) +
  geom_density()

#Plot a histogram and density of the ct_blood variable
ggplot(hospital_df_merged,aes(ct_blood)) +
  geom_histogram()

ggplot(hospital_df_merged,aes(ct_blood)) +
  geom_density()


#Plot a histogram and density of the BMI variable
ggplot(hospital_df_merged,aes(BMI)) +
  geom_histogram()

ggplot(hospital_df_merged,aes(BMI)) +
  geom_density()



# 1.5 Create a scatter plot--------------------------------------------------

# weight vs age
ggplot(hospital_df_merged,aes(x=wt_kg, y=age)) +
  geom_point()

# Plot a scatter of weight vs age and color the points by gender
ggplot(hospital_df_merged,aes(x=wt_kg, y=age, color=gender)) +
  geom_point()

# Plot a scatter of weight vs age and color the points by outcome
ggplot(hospital_df_merged,aes(x=wt_kg, y=age, color=outcome)) +
  geom_point()


# 1.6 Create a scatter plot + line graph--------------------------------------------------
# Plot a scatter of weight vs height then add a line graph
ggplot(hospital_df_merged,aes(x=wt_kg, y=age)) +
  geom_point() +
  geom_line()

# Plot a scatter of weight vs height then add a line graph color by gender: Adjust the size of the dots:Change the line typ
ggplot(hospital_df_merged,aes(x=wt_kg, y=age, color=gender)) +
  geom_point(size=1, color="red") +
  geom_line(size=0.8, color="brown", linetype=2)


# Plot a scatter of weight vs height then add a line graph color the points by gender: Add the theme_bw
ggplot(hospital_df_merged,aes(x=wt_kg, y=age, color=gender)) +
  geom_point(size=1, color="red") +
  geom_line(size=0.8, color="brown", linetype=2) +
  theme_bw()


# 1.7 Create boxplot using ggplot--------------------------------------------------

# Plot a boxplot weight vs gender
ggplot(hospital_df_merged,aes(y=wt_kg, x=gender)) +
  geom_boxplot()

# Plot a boxplot of height by age_group
ggplot(hospital_df_merged,aes(y=ht_m, x=age_cat)) +
  geom_boxplot()

# Plot a boxplot of height by age_group color by gender
ggplot(hospital_df_merged,aes(y=ht_m, x=age_cat, color=gender)) +
  geom_boxplot()

# Plot a box of ct_blood vs chills
ggplot(hospital_df_merged,aes(y=ct_blood, x=chills)) +
  geom_boxplot()

# Plot a box of height by age_group color by gender add scatter. Try adding a layer of theme_bw()
ggplot(hospital_df_merged,aes(y=ht_m, x=age_cat, color=gender)) +
  geom_boxplot()  +
  geom_point()

# 1.7 Create and saving barplot using ggplot--------------------------------------------------

# Plot a barplot of gender
ggplot(hospital_df_merged,aes(x=gender)) +
  geom_bar()

# Plot a barplot of chills
ggplot(hospital_df_merged,aes(x=chills)) +
  geom_bar()

# Plot a barplot of age group and color by gender
ggplot(hospital_df_merged,aes(x=age_cat, fill=gender)) +
  geom_bar()

ggplot(hospital_df_merged,aes(x=age_cat, fill=gender)) +
  geom_bar(position="dodge")


# Export dataset in csv format
export(hospital_df_merged,"data/hosp_df_merged.csv")

# SAVING PLOTS

ht_distribution<-ggplot(hospital_df_merged,aes(y=ht_m, x=age_cat, color=gender)) +
  geom_boxplot()  +
  geom_point()

ggsave("Day 3/graphs/height_distribution.png", plot = ht_distribution, width = 8, height = 6, dpi = 300)
ggsave("Day 3/graphs/height_distribution.pdf", plot = ht_distribution, width = 8, height = 6, dpi = 300)




















