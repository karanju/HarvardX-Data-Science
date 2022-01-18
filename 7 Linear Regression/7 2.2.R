library(HistData)
library(tidyverse)

data("GaltonFamilies")
set.seed(1983)
galton_heights <- GaltonFamilies %>%
  filter(gender == "male") %>%
  group_by(family) %>%
  sample_n(1) %>%
  ungroup() %>%
  select(father, childHeight) %>%
  rename(son = childHeight)


rss <- function(beta0, beta1, data){
  resid <- galton_heights$son - (beta0+beta1*galton_heights$father)
  return(sum(resid^2))
}

beta1 = seq(0, 1, len=nrow(galton_heights))
results <- data.frame(beta1 = beta1,
                      rss = sapply(beta1, rss, beta0 = 25))
results %>% ggplot(aes(beta1, rss)) + geom_line() + 
  geom_line(aes(beta1, rss))


fit <- lm(son ~ father, data = galton_heights)
fit$coef

summary(fit)


B <- 1000
N <- 50
lse <- replicate(B, {
  sample_n(galton_heights, N, replace = TRUE) %>% 
    lm(son ~ father, data = .) %>% 
    .$coef 
})
lse <- data.frame(beta_0 = lse[1,], beta_1 = lse[2,]) 

sample_n(galton_heights, N, replace = TRUE) %>% 
  lm(son ~ father, data = .) %>% 
  summary %>% .$coef

lse %>% summarize(se_0 = sd(beta_0), se_1 = sd(beta_1))

library(gridExtra)
p1 <- lse %>% ggplot(aes(beta_0)) + geom_histogram(binwidth = 5, color = "black") 
p2 <- lse %>% ggplot(aes(beta_1)) + geom_histogram(binwidth = 0.1, color = "black") 
grid.arrange(p1, p2, ncol = 2)


lse %>% summarize(cor(beta_0, beta_1))


B <- 1000
N <- 50
lse <- replicate(B, {
  sample_n(galton_heights, N, replace = TRUE) %>%
    mutate(father = father - mean(father)) %>%
    lm(son ~ father, data = .) %>% .$coef 
})

cor(lse[1,], lse[2,]) 


galton_heights %>% ggplot(aes(son, father)) +
  geom_point() +
  geom_smooth(method = "lm")


beta1 = seq(0, 1, len=nrow(galton_heights))
results <- data.frame(beta1 = beta1,
                      rss = sapply(beta1, rss, beta0 = 36))
results %>% ggplot(aes(beta1, rss)) + geom_line() + 
  geom_line(aes(beta1, rss), col=2)


library(Lahman)

teams = Teams %>% filter(yearID %in% 1961:2001) 
teams = Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(BB_per_game = BB / G, R_per_game = R / G, HR_per_game = HR / G ) 
  
fit <- lm(R_per_game ~ BB_per_game + HR_per_game, data = teams)
fit$coef




set.seed(1989) #if you are using R 3.5 or earlier
set.seed(1989, sample.kind="Rounding") #if you are using R 3.6 or later
library(HistData)
data("GaltonFamilies")
options(digits = 3)    # report 3 significant digits

female_heights <- GaltonFamilies %>%     
  filter(gender == "female") %>%     
  group_by(family) %>%     
  sample_n(1) %>%     
  ungroup() %>%     
  select(mother, childHeight) %>%     
  rename(daughter = childHeight)

fit <- lm(mother ~ daughter, data = female_heights)
fit$coef

head(female_heights)

44.18 + 0.31*69


teams = Teams %>% filter(yearID %in% 1999:2001 & AB >100 ) %>%
  mutate(SinglesRate = (H-HR-X2B-X3B)/G, BB_rate = BB/G) 

head(teams)

t = teams %>% 
  group_by(yearID)%>%
  summarise(mean_singles = mean(SinglesRate), BB_mean = mean(BB_rate)) 

t
t %>% filter (mean_singles> 0.2)

teams %>% 
  group_by(yearID) %>% 
  summarise(BB_rate = sum(BB_rate), SinglesRate=sum(SinglesRate))


library(Lahman)
bat_02 <- Batting %>% filter(yearID == 2002) %>%
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa >= 100) %>%
  select(playerID, singles, bb)
bat_02

pl <- Batting %>% filter(yearID %in% 1999:2001) %>%
  group_by(yearID) %>%
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa >= 100) %>%
  select(yearID,playerID, singles, bb)

pl

pm = aggregate(pl[, 3:4], list(pl$playerID), mean)
pm = pm %>%
  rename(playerID = Group.1)

head(bat_02)
head(pm)

p = inner_join(pm,bat_02)
p
p = inner_join(pl,bat_02)


library(plyr)

pn = ddply(pl, "playerID" , summarize,  Rate1=mean(singles), Rate2=mean(bb))
pn
pl
p = inner_join(pl,pn)
p
p1 = inner_join(pn,bat_02)
p1  %>% summarize(r = cor(Rate1  , singles))
p1  %>% summarize(r = cor(Rate2  , bb))
p1

p %>% ggplot(aes(singles, Rate1)) +  geom_point()
p %>% ggplot(aes(bb , Rate2)) +  geom_point()

p
fit <- lm(singles ~ Rate1, data = p1)
fit$coef

fit <- lm(bb ~ Rate2, data = p1)
fit$coef
