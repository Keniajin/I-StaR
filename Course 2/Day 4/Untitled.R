#############################################
# Summarising data
# Comparison of means and proportions
#############################################

library(tidyverse)  



# import dataset saved from previous exercise
new_hosp_df_merged <- import("data/hosp_df_merged.csv")


# Explore summary statistics for age, height and weight, BMI
new_hosp_df_merged %>%
  summarise(
    age = mean(Sepal.Length),
    wt_kg = mean(Sepal.Width),
    ht_cm = sd(Sepal.Length),
    ct_blood = sd(Sepal.Width),
    min_sepal_length = min(Sepal.Length),
    max_sepal_length = max(Sepal.Length),
    min_sepal_width = min(Sepal.Width),
    max_sepal_width = max(Sepal.Width)
  )