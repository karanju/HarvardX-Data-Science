library(tidyverse)
library(dslabs)
d <- 0.039
Ns <- c(1298, 533, 1342, 897, 774, 254, 812, 324, 1291, 1056, 2172, 516)
p <- (d + 1) / 2

polls <- map_df(Ns, function(N) {
  x<- sample(c(0,1), size=N, replace=TRUE, prob=c(1-p, p))
  x_hat <- mean(x)
  se_hat <- sqrt(x_hat * (1 - x_hat) / N)
  list(estimate = 2 * x_hat - 1, 
       low = 2*(x_hat - 1.96*se_hat) - 1, 
       high = 2*(x_hat + 1.96*se_hat) - 1,
       sample_size = N)
}) %>% mutate(poll = seq_along(Ns))
polls

d_hat <- polls %>% 
  summarize(avg = sum(estimate*sample_size) / sum(sample_size)) %>% 
  pull(avg)

p_hat <- (1+d_hat)/2
moe <- 2*1.96*sqrt(p_hat*(1-p_hat)/sum(polls$sample_size))   
round(d_hat*100,1)
round(moe*100, 1)

polls <- polls_us_election_2016 %>% 
  filter(state == "U.S." & enddate >= "2016-10-31" &
           (grade %in% c("A+","A","A-","B+") | is.na(grade)))

polls <- polls %>% 
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)

polls

d_hat <- polls %>% 
  summarize(d_hat = sum(spread * samplesize) / sum(samplesize)) %>% 
  pull(d_hat)

p_hat <- (d_hat+1)/2 
moe <- 1.96 * 2 * sqrt(p_hat * (1 - p_hat) / sum(polls$samplesize))
moe

one_poll_per_pollster <- polls %>% group_by(pollster) %>% 
  filter(enddate == max(enddate)) %>%
  ungroup()
qplot(spread, data = one_poll_per_pollster, binwidth = 0.01)

sd(one_poll_per_pollster$spread)

results <- one_poll_per_pollster %>% 
  summarize(avg = mean(spread), 
            se = sd(spread) / sqrt(length(spread))) %>% 
  mutate(start = avg - 1.96 * se, 
         end = avg + 1.96 * se) 
round(results * 100, 1)
