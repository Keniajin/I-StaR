#     This script is for demonstrating how use some the functions within the 
#         tidyverse array of packages to sumamrize data.
#     This is by using the group_by() and summarise() functions. 


#     Written by M. Otiende
#     05 May 2021


# load the tidyverse and gapminder packages
library(gapminder)
library(tidyverse)


# Task 1 - what is the mean life expectancy in each continent
continent_life_exp <- gapminder %>%
  group_by(continent) %>%
  summarise(mean_exp=mean(lifeExp), 
            median_exp=median(lifeExp), 
            min_exp=min(lifeExp), 
            max_exp=max(lifeExp))

continent_life_exp

# Task 2 - what is the mean life expectancy a) including asia and b) excluding asia
## part a
life_exp_including_asia <- gapminder %>%
  summarise(mean_exp=mean(lifeExp))

life_exp_including_asia

# part b
life_exp_other_than_asia <- gapminder %>%
  filter(continent!="Asia") %>%
  summarise(mean_exp=mean(lifeExp))

life_exp_other_than_asia


# Task 3 - create a summary table showing the population in 2002 in 
#           each continent and the minimum and maximum gdpPercap in each
#           continent.


pop_2002 <- gapminder %>%
  filter(year==2002) %>%
  group_by(continent) %>%
  summarise(total_pop=sum(pop), min_gdp=min(gdpPercap), max_gdp=max(gdpPercap))

pop_2002






