
#############################################
# Data wrangling
# Script for cleaning ad simmarizing the BP data collected during the class sessions
# Reshaping the data
# Mark Otiende | Kennedy Mwai
#############################################

# Load packages -----------------------------------------------------------
library(rio)
library(epikit)
library(gtsummary)
library(lubridate)
library(gapminder)
library(tidyverse)

# Question 1  Import data --------------------------------------------------------------
## Import data -------------------------------------------------------------
bp_df <- import("data/BP_measure_updated.xlsx")



# Question 2 Data cleaning ----------------------------------------------


## Create a new variable (name it hr_cat) that classifies hr as high or low using a cutt-off of 72.

bp_clean <- bp_df %>% 
  ## Clean the syst_diast variable to separate the diastolic and systolic measurements.
  separate(syst_diast ,
           into = c("syst2","diast2"),
           sep="/",
           # keep the original variable, if you put FALSE it drops it
           remove = FALSE ) %>% 
  # Create a new variable (name it hr_cat)
  mutate(hr_cat=ifelse(hr>72, 
                       "HIGH","LOW"))



# Question 3 Plotting graphs -----------------------------------------------
### Make a boxplot that compares hr by age_cat

p1 <- ggplot(bp_clean , aes(y=hr,x=age_cat))  +
  geom_boxplot(fill='#00FFFF') +
  labs(y="Heart rate", x="Age category",title =  "Heart rate by age category") +
  ## adding a theme
  theme_bw() +
  ## putting the title at the middle
  theme(plot.title = element_text(hjust = 0.5))

p1


## • Make a boxplot that compares hr by sex. Make this one horizontal.

p2 <- ggplot(bp_clean , aes(x=hr,y=sex))  +
  geom_boxplot(fill='#00FFFF') +
  labs(y="Heart rate", x="Sex",title =  "Heart rate by sex") +
  ## adding a theme
  theme_bw() +
  ## putting the title at the middle
  theme(plot.title = element_text(hjust = 0.5))

p2


#• Make a bar graph that shows the fequencies of male and female respondents


p3 <- ggplot(bp_clean , aes(sex))  +
  geom_bar(fill='#00FFFF') +
  labs(y="Count", x="Sex",title =  "Sex count") +
  ## adding a theme
  theme_bw() +
  ## putting the title at the middle
  theme(plot.title = element_text(hjust = 0.5))

p3



# Question 4 Data summaries --------------------------------------------------------
## • Make data summaries that shows the mean hr by sex. Hint: use group_by() and summarise()

mean_hr_sex <- bp_clean %>% 
  group_by(sex) %>% 
  summarise(mean_hr=mean(hr))
mean_hr_sex


## • Make data summaries that shows the mean hr by age_cat.


mean_hr_age <- bp_clean %>% 
  group_by(age_cat) %>% 
  summarise(n=n(), mean_hr=mean(hr))
mean_hr_age



# Question 5 Comparison of means and proportions --------------------------

## Compare mean hr between males and females and interpret.

t.test(data=bp_clean ,
       hr ~sex)


## Compare proportions of or respondents with high hr in males and females. 
infer::prop_test(bp_clean ,
                hr_cat  ~ sex, 
                order = c("female",
                          "male"))

# Question 6 Reshaping --------------------------------------------------------------

gapminder <- gapminder

# create a small dataset of East Africa pop

east_africa <- gapminder %>% 
  filter(country %in% c('Kenya','Uganda','Tanzania', 'Burundi', 'Rwanda')) %>% 
  select(country,year,pop)
glimpse(east_africa)


## Create a dataframe where the country appear as columns
ea_wider <- east_africa %>% 
  pivot_wider(names_from = 'country' , values_from = 'pop')


ggplot(ea_wider ,aes(x=year,y=Kenya)) +
  geom_line(linewidth=2 , color="red") +
  geom_point() +
  labs(y="Population", x="Year",title =  "Kenya Population over time") +
  ## adding a theme
  theme_bw() +
  ## putting the title at the middle
  theme(plot.title = element_text(hjust = 0.5))

ggplot(ea_wider ,aes(x=year,y=Burundi)) +
  geom_line(linewidth=2 , color="red") +
  geom_point() +
  labs(y="Population", x="Year",title =  "Burundi Population over time") +
  ## adding a theme
  theme_bw() +
  ## putting the title at the middle
  theme(plot.title = element_text(hjust = 0.5))



## compare values in different countries in tthe same year
ea_wider2 <- east_africa %>% 
  pivot_wider(names_from = 'year' , values_from = 'pop' ,
              names_prefix = "yr_")

ggplot(ea_wider2 ,aes(x=country,y=yr_1952)) +
  geom_col() +
  labs(y="Population in 1952", x="Country",title =  "Population of EA in 1952") +
  ## adding a theme
  theme_minimal() +
  ## putting the title at the middle
  theme(plot.title = element_text(hjust = 0.5))


## Pivot longer --> you want to plot for all the countries
ea_longer <- ea_wider %>% 
  pivot_longer(cols = !year , names_to = "country" , values_to = "pop")


ggplot(ea_longer ,aes(x=year,y=pop, color=country, group=country)) +
  geom_line(linewidth=2 ) +
  geom_point() +
  labs(y="Population", x="Year",title =  "EA Population over time") +
  ## adding a theme
  theme_bw() +
  ## putting the title at the middle
  theme(plot.title = element_text(hjust = 0.5))
