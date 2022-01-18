library(Lahman)
library(tidyverse)
library(dbplyr)

dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR = round(HR/G, 1), 
         BB = BB/G,
         R = R/G) %>%
  select(HR, BB, R) %>%
  filter(HR >= 0.4 & HR<=1.2) 

dat
dat %>%  
  group_by(HR) %>%
  summarize(slope = cor(BB,R)*sd(R)/sd(BB))

dat %>%  
  group_by(HR) %>%
  lm(R ~ BB, data = .) %>% .$coef


dat %>% group_by(HR) %>% head()
dat %>% group_by(HR) %>% class()


dat %>%  
  group_by(HR) %>%
  do(fit = lm(R ~ BB, data = .))


get_slope <- function(data){
  fit <- lm(R ~ BB, data = data)
  data.frame(slope = fit$coefficients[2], 
             se = summary(fit)$coefficient[2,2])
}

dat %>%  
  group_by(HR) %>%
  do(get_slope(.))


get_lse <- function(data){
  fit <- lm(R ~ BB, data = data)
  data.frame(term = names(fit$coefficients),
             slope = fit$coefficients, 
             se = summary(fit)$coefficient[,2])
}

dat %>%  
  group_by(HR) %>%
  do(get_lse(.))


library(broom)
fit <- lm(R ~ BB, data = dat)
tidy(fit)

tidy(fit, conf.int = TRUE)


dat %>%  
  group_by(HR) %>%
  do(tidy(lm(R ~ BB, data = .), conf.int = TRUE)) %>%
  filter(term == "BB") %>%
  select(HR, estimate, conf.low, conf.high) %>%
  ggplot(aes(HR, y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_errorbar() +
  geom_point()

glance(fit)


dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR = HR/G,
         R = R/G) %>%
  select(lgID, HR, BB, R) 

dat %>% 
  group_by(lgID) %>% 
  do(tidy(lm(R ~ HR, data = .), conf.int = T)) %>% 
  filter(term == "HR") 




library(tidyverse)
library(HistData)
library(dplyr)
data("GaltonFamilies")
set.seed(1) # if you are using R 3.5 or earlier
set.seed(1, sample.kind = "Rounding") # if you are using R 3.6 or later
galton <- GaltonFamilies %>%
  group_by(family, gender) %>%
  sample_n(1) %>%
  ungroup() %>% 
  gather(parent, parentHeight, father:mother) %>%
  mutate(child = ifelse(gender == "female", "daughter", "son")) %>%
  unite(pair, c("parent", "child"))

galton
galton %>% 
  group_by("pair") %>%
  summarise(r = cor(parentHeight, childHeight ))

ddply(galton, "pair" , summarize,  r=cor(parentHeight, childHeight ))

galton %>% 
  group_by(pair) %>% 
  do(tidy(lm(childHeight ~ parentHeight, data = .), conf.int = T)) %>%
  filter(term == "parentHeight") 

galton %>% 
  group_by(pair) %>% 
  do(tidy(lm(childHeight ~ parentHeight, data = .), conf.int = T)) %>%
  filter(term == "parentHeight") %>%
  select(pair, estimate, conf.low, conf.high) %>%
  ggplot(aes(pair, y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_errorbar() +
  geom_point()
