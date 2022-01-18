library(tidyverse)
library(ggrepel)
library(dslabs)
ds_theme_set()
data(murders)
head(murders)

data(polls_us_election_2016)
head(results_us_election_2016)
identical(results_us_election_2016$state, murders$state)

tab <- left_join(murders, results_us_election_2016, by = "state")
head(tab)

tab %>% ggplot(aes(population/10^6, electoral_votes, label = abb)) +
  geom_point() +
  geom_text_repel() + 
  scale_x_continuous(trans = "log2") +
  scale_y_continuous(trans = "log2") +
  geom_smooth(method = "lm", se = FALSE)

tab1 <- slice(murders, 1:6) %>% select(state, population)
tab1
tab2 <- slice(results_us_election_2016, c(1:3, 5, 7:8)) %>% select(state, electoral_votes)
tab2

left_join(tab1, tab2)
tab1 %>% left_join(tab2)
tab1 %>% right_join(tab2)
inner_join(tab1, tab2)
semi_join(tab1, tab2)
anti_join(tab1, tab2)


bind_cols(a = 1:3, b = 4:6)

tab1 <- tab[, 1:3]
tab2 <- tab[, 4:6]
tab3 <- tab[, 7:9]
new_tab <- bind_cols(tab1, tab2, tab3)
head(new_tab)

tab1 <- tab[1:2,]
tab2 <- tab[3:4,]
bind_rows(tab1, tab2)

head(tab2)

intersect(1:10, 6:15)
intersect(c("a","b","c"), c("b","c","d"))
tab1 <- tab[1:5,]
tab2 <- tab[3:7,]
intersect(tab1, tab2)

union(1:10, 6:15)
union(c("a","b","c"), c("b","c","d"))
tab1 <- tab[1:5,]
tab2 <- tab[3:7,]
union(tab1, tab2)

setdiff(1:10, 6:15)
setdiff(6:15, 1:10)
tab1 <- tab[1:5,]
tab2 <- tab[3:7,]
setdiff(tab1, tab2)

setequal(1:5, 1:6)
setequal(1:5, 5:1)
setequal(tab1, tab2)

dim(tab1)
dim(tab2)
tab1

library(Lahman)
top <- Batting %>% 
  filter(yearID == 2016) %>%
  arrange(desc(HR)) %>%    # arrange by descending HR count
  slice(1:10)    # take entries 1-10
top %>% as_tibble()

Master %>% as_tibble()

head(Master)

top_names <- top %>%left_join(Master) %>%
  select(playerID, nameFirst, nameLast, HR)

top_names

Salaries

head(AwardsPlayers) 

aw <- AwardsPlayers %>% 
  filter(yearID == 2016) 
head(aw)
aw

dat <- top_names %>%inner_join(aw) 
dat
intersect(top_names,aw)
set

dat <- aw %>%anti_join(top_names)
dat 
top_names
tab1 = top_names %>% select(playerID)
tab1
tab2 = aw %>% select(playerID)
tab2
intersect(tab1,tab2)
setdiff(tab1,tab2)
setdiff(tab2,tab1)
union(tab1,tab2)
a = tab2[tab2$playerID == "trumbma01"]
a
