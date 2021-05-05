## Mutate 
## Created by 
library(gapminder)
library(tidyverse)


## check the names of your data
names(gapminder)

## glimpse of your data
glimpse(gapminder)

## creatte gdp variable

gapminder_new <- mutate(gapminder,
                        gdp=gdpPercap*pop ,
                        pop_mil=pop / 100000,
                      pop_mil2 = round(pop / 100000))


## use the if else command to create a categorical variable
## to identify after and before 2000

gapminder_new <- mutate(gapminder,
                after_2000 = ifelse(year > 2000,
                           "After 2000",
                           "Before 2000"))

## Add an africa column that is TRUE if the country 
#is on the African continent

gapminder_new <- mutate(gapminder,
                        africa= ifelse(continent=="Africa",
                                       TRUE,
                                       FALSE))
## Add a column for logged GDP per capita (hint: use log())

gapminder_new2 <- mutate(gapminder,
                        logged_gdpPerCap=log(gdpPercap) )

## solution task 3
##Add an africa_asia column that says 
#“Africa or Asia” if the country is in Africa or Asia, and “Not
## Africa or Asia” if it’s not
gapminder3 <- mutate(gapminder,
                     africa_asia = ifelse(continent=="Africa" | continent=="Asia",
                                          "Africa or Asia",
                                          "Not Africa nor Asia"))


### pipe operator to combine different verbs
## filtering for 2002 and creating the log for gdp per cap
gapminder_2002_log <- gapminder %>%
  ## filter 2002
  filter(year == 2002) %>%
  ## create a new variable log gdp
  mutate(log_gdpPercap = log(gdpPercap))


## create a dataset for after 2000
## creat a variable pop_mil for population in millions
gapdf_after_2000 <- gapminder %>%
  ## filter after 2000
  filter(year > 2000) %>%
  ## create a new variable pop millions
  mutate(pop_mil = pop / 1000000) %>% 
  ## round off pop_mil
  mutate(pop_mil = round(pop_mil)) 
