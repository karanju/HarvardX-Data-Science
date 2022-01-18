library(tidyverse)
options(digits = 3)

# load brexit_polls object
library(dslabs)
data(brexit_polls)

p <- 0.481    # official proportion voting "Remain"
d <- 2*p-1    # official spread
N = 1500

X = p*N 
X
se = sqrt(p*(1-p)*N)
se

X_hat = 0.481
se_hat = sqrt(p*(1-p)/N)
se_hat
d
2*se

head(brexit_polls)
brexit_polls <- brexit_polls %>%
  mutate(x_hat = (spread+1)/2)

head(brexit_polls)
mean(brexit_polls$spread)
sd(brexit_polls$spread)
sd(brexit_polls$spread)/sqrt(sum(brexit_polls$samplesize))
mean(brexit_polls$x_hat)
sd(brexit_polls$x_hat)

brexit_polls[1,]
brexit_polls[[1,10]]
se = sqrt(0.52*0.48/4772)
c( brexit_polls[[1,10]] - qnorm(0.975)*se
, brexit_polls[[1,10]] + qnorm(0.975)*se
 )
qnorm(0.975)*0.0129

library(tidyverse)

# load brexit_polls object and add x_hat column
library(dslabs)
data(brexit_polls)
brexit_polls <- brexit_polls %>%
  mutate(x_hat = (spread + 1)/2)

# final proportion voting "Remain"
p <- 0.481
june_polls = brexit_polls %>% 
  filter( enddate >= "2016-06-01") 
june_polls
june_polls <- june_polls %>%
  mutate(se_x_hat = sqrt(x_hat*(1-x_hat)/samplesize) ) %>%
  mutate(sp_se_x_hat = 2*se_x_hat ) %>%
  mutate(lower = spread - qnorm(0.975)*sp_se_x_hat, upper = spread + qnorm(0.975)*sp_se_x_hat) %>%
  mutate(hit = -0.038>=lower & -0.038<=upper) 


z = june_polls %>% filter(0>=lower & 0<=upper)
z
z = june_polls %>% filter(lower>0)
z = june_polls %>% filter(-0.038>=lower & -0.038<=upper)

z = june_polls %>% group_by(pollster) %>%
             summarize(avg = mean(hit),n = n()) %>%
             arrange(desc(avg))
june_polls %>% ggplot(aes(x=poll_type,y=spread)) + geom_boxplot()

combined_by_type <- june_polls %>%
  group_by(poll_type) %>%
  summarize(N = sum(samplesize),
            spread = sum(spread*samplesize)/N,
            p_hat = (spread + 1)/2)
combined_by_type = combined_by_type %>%mutate(se = sqrt(p_hat*(1-p_hat)/N)) %>%
                    mutate(lower = spread - qnorm(0.975)*2*se, upper = spread + qnorm(0.975)*2*se)
combined_by_type


library(tidyverse)

# load brexit_polls object and add x_hat column
library(dslabs)
data(brexit_polls)
brexit_polls <- brexit_polls %>%
  mutate(x_hat = (spread + 1)/2)

# final proportion voting "Remain"
p <- 0.481

brexit_hit <- brexit_polls %>%
  mutate(p_hat = (spread + 1)/2,
         se_spread = 2*sqrt(p_hat*(1-p_hat)/samplesize),
         spread_lower = spread - qnorm(.975)*se_spread,
         spread_upper = spread + qnorm(.975)*se_spread,
         hit = spread_lower < -0.038 & spread_upper > -0.038) %>%
  select(poll_type, hit)
brexit_hit
z =  brexit_hit %>% filter(poll_type == "Online")
z1 = z %>% filter(hit==TRUE)
y =  brexit_hit %>% filter(poll_type == "Telephone")
y
y1 = y %>% filter(hit==TRUE)
y1
z1
count(z,hit=FALSE)
mean(z$hit)
85-48
two_by_two <- data.frame(awarded = c("yes", "no"), 
                         Online = c(48, 37),
                         Telephone = c(10, 32))
two_by_two
chisq_test <- two_by_two %>% select(-awarded) %>% chisq.test()
chisq_test$p.value

odds_men <- with(two_by_two, (Online[1]/sum(Online)) / (Online[2]/sum(Online)))
odds_men
odds_tel <- with(two_by_two, (Telephone[1]/sum(Telephone)) / (Telephone[2]/sum(Telephone)))
odds_tel
odds_men - odds_tel
odds_men / odds_tel

brexit_polls 
brexit_polls %>% ggplot(aes(x=enddate,y=spread,color=poll_type)) + geom_smooth(span = 0.3) + geom_point() + geom_hline(yintercept= -.038)
brexit_polls

brexit_long <- brexit_polls %>%
  gather(vote, proportion, "remain":"undecided") %>%
  mutate(vote = factor(vote))
brexit_long

brexit_long %>% ggplot(aes(x=enddate,y=proportion,color=vote)) + geom_smooth(span = 0.3) + geom_hline(yintercept= -.038)

