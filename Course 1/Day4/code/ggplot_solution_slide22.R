
## ggplot 2 
## Created by Ken to reproduce slide 22 in ggplot 2 presentation
#'- Can you make the shape of the points in the scatter plot to vary with ethnicity?
#'- add a scale shape attribute. Hint use: scale_shape_discrete(name="legend title")
#'- Instead of having multiple smoothing lines for each ethnic group, integrate them all under one line.

library(tidyverse)

##set wd

## read in the data
bw_df <- read_csv('data/birthweight2.csv')


## create a new variable ethnic variable
bw_df <- bw_df %>% 
  mutate(ethnic2=as.factor(ethnic)) 



## create the plot
  ggplot(data=bw_df, aes(x = gestwks,y = bweight)) +
    ## add the point aesthetics , color by sex and shape using ethnicity
  geom_point(aes(color = sex,shape=ethnic2)) +
    ## add the smooth line
  geom_smooth(method = "lm") +
    ## facet wrap by sex
  facet_wrap(~sex, ncol = 2) +
    ## add the lables
  labs(x = "Gestation weeks", y = "Birthweight",
       color = "Sex",
       shape= "Ethnicity",
       title = "Lower gestation weeks leads to low birthweight",
       subtitle = "Birth weight is in grams",
       caption = "Is there something we can observe by gender") +
    ## add the theme
  theme_bw() +
  ## name the legend
  scale_color_discrete(name="Sex") + 
    scale_shape_discrete(name="Ethnicity") +
    ### increase the font size
    theme(plot.title=element_text(size=15, face="bold"), 
          axis.text.x=element_text(size=18), 
          axis.text.y=element_text(size=18),
          axis.title.x=element_text(size=20),
          axis.title.y=element_text(size=20)) 
  