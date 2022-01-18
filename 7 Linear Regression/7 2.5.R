library(tidyverse)
library(broom)
library(Lahman)

Teams_small <- Teams %>% 
  filter(yearID %in% 1961:2001) %>% 
  mutate(avg_attendance = attendance/G, R = R/G)
Teams_small

Teams_small %>%   
  do(tidy(lm(avg_attendance ~ R, data = .), conf.int = T))
  
Teams_small <- Teams %>% 
  filter(yearID %in% 1961:2001) %>% 
  mutate(avg_attendance = attendance/G, HR = HR/G)
Teams_small

Teams_small %>%   
  do(tidy(lm(avg_attendance ~ HR, data = .), conf.int = T))

view(Teams)
Teams_small <- Teams %>% 
  filter(yearID %in% 1961:2001) %>% 
  mutate(avg_attendance = attendance/G, HR = HR/G)
Teams_small

Teams_small %>% 
  do(tidy(lm(avg_attendance ~ W, data = .), conf.int = T))

Teams_small %>% 
  do(tidy(lm(avg_attendance ~ yearID, data = .), conf.int = T))

Teams_small %>% 
  do(tidy(lm(avg_attendance ~ W, data = .), conf.int = T))

Teams_small %>% mutate( R = R/G) %>%
  summarize(r = cor(W, R) )

Teams_small %>% mutate( HR = HR) %>%
  summarize(r = cor(W, HR) )

ts = Teams_small %>% 
  mutate( W_strat = factor(round(W/10)) ) %>% 
  filter(W_strat %in% 5:10) 

ts %>% 
  group_by(W_strat) %>% mutate(R = R/G) %>%
  do(tidy(lm(avg_attendance ~ R, data = .), conf.int = T))

ts %>% 
  group_by(W_strat) %>% mutate(HR = HR/G) %>%
  do(tidy(lm(avg_attendance ~ HR, data = .), conf.int = T))

ts %>% 
  group_by(W_strat)  %>%
  do(tidy(lm(avg_attendance ~ HR, data = .), conf.int = T))

fit = Teams_small %>% mutate(HR = HR/G) %>% 
  lm(avg_attendance ~ R + HR + W + yearID, data = .)

fit 
test = data.frame(R=5,HR=1.2,W=80,yearID=2002)
test
avg_attend = predict(fit, test)
avg_attend


ts = Teams %>% filter(yearID == 2002) %>% 
  mutate(avg_attendance = attendance/G, R = R/G, HR = HR/G)
ts
p_avg = predict(fit, ts)

p_avg

ac_avg = Teams %>% 
          filter(yearID == 2002) %>% 
          mutate(avg_attendance = attendance/G) %>%
          select(avg_attendance)
ac_avg

r = cor(p_avg,ac_avg)
r
