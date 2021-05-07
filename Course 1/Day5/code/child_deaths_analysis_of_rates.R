setwd("")

# CALCULATING RATES 
# 1.0 Load packages
library(epiDisplay)
library(epitools)
library(epiR)
library(fmsb)
library(tidyverse)


# 2.0 Load the child deaths data
childdeaths <- read_csv("data/child.deaths.csv")
names(childdeaths)

# 2.1 label the values of the variable "status"
childdeaths <- childdeaths %>%
  mutate(status=factor(childdeaths$status,
                levels = c(1,2),
                labels = c("alive", "dead")))
table(childdeaths$status)

# 3.0 generate a column with person-years by first formating the dates of birth and exit
childdeaths <- childdeaths %>%
  mutate(date_exit = as.Date(childdeaths$date_exit, "%m/%d/%y"), 
         date_birth = as.Date(childdeaths$date_birth, "%m/%d/%y"),
         person_yrs = difftime(date_exit,date_birth),
         person_yrs = as.numeric(person_yrs)/365.25)

# 4.0 Calculate rates
total_deaths<-length(childdeaths$status[childdeaths$status=="dead"])
total_deaths

total_pyrs<-sum(childdeaths$person_yrs)
total_pyrs
mort_rate<-(as.numeric(total_deaths)/total_pyrs)
mort_rate

# 5.0 calculate standard error of the rate
s.e.Rate<-sqrt(total_deaths)/total_pyrs*1000 # 
s.e.Rate
s.e.log.Rate<-1/sqrt(total_deaths)
s.e.log.Rate

# 6.0 Compute 95% CI for rate
log.rate=log(mort_rate)
lower.bound=exp(log.rate-1.96*s.e.log.Rate)
lower.bound
upper.bound=exp(log.rate+1.96*s.e.log.Rate)
upper.bound



# COMPARING TWO RATES
# 7.0 Rate difference: Males vs. Females

# 7.1 Calculate number of deaths for each gender: male and female
f.deaths<-length(childdeaths$status[childdeaths$status=="dead" & childdeaths$sex=="f"])
f.deaths

m.deaths<-length(childdeaths$status[childdeaths$status=="dead" & childdeaths$sex=="m"])
m.deaths

# 7.2  calculate person years for each gender
f.total_pyrs<-sum(childdeaths$person_yrs[childdeaths$sex=="f"])
f.total_pyrs

m.total_pyrs<-sum(childdeaths$person_yrs[childdeaths$sex=="m"])
m.total_pyrs

table(childdeaths$status,childdeaths$sex)

male_vs_female<-ratetable(f.deaths,m.deaths,f.total_pyrs,m.total_pyrs) 
dimnames(male_vs_female)<-list(Exposure=c("males","females"), Outcome=c("deaths","pyears"))
male_vs_female

# 7.3 calculate rate difference

gender_diff<-ratedifference(f.deaths,m.deaths,f.total_pyrs,m.total_pyrs,conf.level=0.95)
gender_diff

# 7.4 calculate rate ratio
rateratio.wald(male_vs_female,rev="r")

