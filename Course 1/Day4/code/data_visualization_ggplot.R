#------------------------------------------------------------------------------#


#     This R script is for plotting a variety of graphs in R using the ggplot
#     package.
#     
#     Written by M.Otiende
#     05 May 2021
#     
#------------------------------------------------------------------------------#

# We will be working with the gapminder dataset so there is no need to set a
# directory for now

# load packages
library(tidyverse)
library(gapminder)


# Graphs for numerical data
gapminder %>%
  ggplot(aes(x=lifeExp)) + 
  geom_histogram(binwidth=1, color = 'white') +  # what happens when you delete the whole of ",color = 'white' " including the comma?
  theme_bw()

## density
gapminder %>%
  ggplot(aes(x=lifeExp)) + 
  geom_density(fill='grey') +  # what happens when you delete the whole of ",color = 'white' " including the comma?
  theme_bw()


## box plot (get illustration from here)
gapminder %>%
  ggplot( aes(y=lifeExp)) +
  geom_boxplot() +
  scale_x_discrete() +
  ylim(c(0,100)) +
  labs(title="A boxplot with jitter", y= "Life Expectancy")
  xlab("")


## scatter

gapminder %>%
  ggplot(aes(x=lifeExp, y=gdpPercap)) +
  geom_point()
  
  
## violin plot
gapminder %>%
  ggplot(aes(x="combined", y=lifeExp)) +
  scale_x_discrete() +
  geom_violin()

## line plot
for_line_plot <- gapminder %>%
  group_by(year) %>%
  summarise(mean_exp=mean(lifeExp)) 

ggplot(for_line_plot, aes(x=year, y=mean_exp)) +
  geom_line() +
  ylim(c(0,100)) +
  labs(title="life expectancy by year",x="year",y="mean life expectancy")



## area plot
for_area_plot <-  gapminder %>%
  group_by(year) %>%
  summarise(mean_exp=mean(lifeExp)) 

ggplot(for_area_plot, aes(x=year, y=mean_exp)) +
  geom_area(fill="grey") +
  ylim(c(0,100)) 
  


# Graphs for categorical data
## bar plot
gapminder %>%
  mutate(pop_per_million=pop/1000000) %>%
  group_by(continent) %>%
  summarize(total_pop = sum(pop_per_million)) %>%
  ggplot(aes(x=continent, y=total_pop)) + 
  geom_bar(stat = "identity")


## grouped bar plot
gapminder %>%
  mutate(period=ifelse(year<1980,"before 1980", "post 1980"), pop_per_million=pop/1000000) %>%
  group_by(continent, period) %>%
  summarise(total_pop=sum(pop_per_million)) %>%
  ggplot(aes(fill=period, y=total_pop, x=continent)) + 
  geom_bar(position="dodge", stat="identity")



## stacked bar plot
### frequencies
gapminder %>%
  group_by(year, continent) %>%
  mutate(pop_per_million=pop/1000000) %>%
  summarise(total_pop=sum(pop_per_million)) %>%
  ggplot(aes(fill=continent, y=total_pop, x=year)) + 
  geom_bar(position="stack", stat="identity")


###percent
gapminder %>%
  group_by(year, continent) %>%
  mutate(pop_per_million=pop/1000000) %>%
  summarise(total_pop=sum(pop_per_million)) %>%
  ggplot(aes(fill=continent, y=total_pop, x=year)) + 
  geom_bar(position="fill", stat="identity")

## pie chart - a terrible way to visualize categorical data and we choose not to promote a bad culture :)

  


 
