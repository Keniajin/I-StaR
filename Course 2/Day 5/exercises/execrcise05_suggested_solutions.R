
#############################################
# Data wrangling
# Script for exploratory data analysis (linear regression)
# Mark Otiende | Kennedy Mwai
#############################################

# Load packages -----------------------------------------------------------
library(rio)
library(epikit)
library(gtsummary)
library(lubridate)
library(gapminder)
library(tidyverse)
library(infer)
library(janitor)

# Question 1:  Importing the dataset into R and clean names

happy <- import("data/WHR2018.csv") %>% 
  clean_names() # Clean the all column names to remove spaces betwen the names


# Question 2: Subset the imported dataset and onle keep the following variables
happy_sub <- happy %>% 
  select(country,year,freedom_to_make_life_choices,confidence_in_national_government,positive_affect,negative_affect)

# Question 3 Dropping records with missing data -----------------------------------------------
# In all the columns selected above, remove records that have missing data.

happy_sub <- happy_sub %>% 
  filter(country!="",                                  # ask for clarification if lost
         !is.na(freedom_to_make_life_choices),
         !is.na(confidence_in_national_government),
         !is.na(positive_affect),
         !is.na(negative_affect))

# Question 4 Scatter plot, correlation coefficient and line of best fit  -----------------------------------------------

# Make a scatter plot of negative_affect vs. positive_affect

p1 <- ggplot(happy_sub, aes(x=negative_affect, y=positive_affect)) + 
  geom_point()

p1
# Calculate and interpret the correlation coefficient of negative_affect vs. positive_affect

cor(happy_sub$negative_affect, happy_sub$positive_affect) # interpret the correlation coefficient


# Add a line of best fit. HINT: add a layer of geom_smooth()

p2 <- ggplot(happy_sub, aes(x=negative_affect, y=positive_affect)) + 
  geom_point() +
  geom_smooth(method = lm, se = FALSE)

p2

# Question 5 Linear regression  -----------------------------------------------

# Fit a linear regression model on negative_affect vs. positive_affect

model_1 <- lm(negative_affect ~ positive_affect, data=happy_sub)

summary(model_1) # interpret this output









 
