library(tidyverse)
library(dslabs)
data(stars)
options(digits = 3) 
stars
mean(stars$magnitude)
sd(stars$magnitude)

stars %>%
  ggplot(aes(x=magnitude)) + geom_density()

stars %>%
  ggplot(aes(x=temp)) + geom_density()

stars %>%
  ggplot(aes(x=temp,y=magnitude)) + geom_point() +
  scale_y_reverse() + 
  scale_x_continuous(trans = "log10") +
  scale_x_reverse() 

library(ggrepel)
stars %>%
  filter(temp>5000 & magnitude > 0) %>%
  ggplot(aes(x=temp,y=magnitude,label=star)) + geom_point() +
  scale_y_reverse() + 
  scale_x_continuous(trans = "log10") +
  geom_text_repel() 
  
stars %>%
  filter(temp<10000 & magnitude < 0) %>%
  ggplot(aes(x=temp,y=magnitude,label=star)) + geom_point() +
  scale_y_reverse() + 
  scale_x_continuous(trans = "log10") +
  geom_text_repel() 

stars %>%
  filter(temp>20000) %>%
  ggplot(aes(x=temp,y=magnitude,color=type)) + geom_point() +
  scale_y_reverse() + 
  scale_x_continuous(trans = "log10")




library(tidyverse)
library(dslabs)
data(temp_carbon)
data(greenhouse_gases)
data(historic_co2)