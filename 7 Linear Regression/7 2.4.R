fit <- Teams %>% 
  filter(yearID %in% 1961:2001) %>% 
  mutate(BB = BB/G, HR = HR/G,  R = R/G) %>%  
  lm(R ~ BB + HR, data = .)
tidy(fit, conf.int = TRUE)

fit <- Teams %>% 
  filter(yearID %in% 1961:2001) %>% 
  mutate(BB = BB / G, 
         singles = (H - X2B - X3B - HR) / G, 
         doubles = X2B / G, 
         triples = X3B / G, 
         HR = HR / G,
         R = R / G) %>%  
  lm(R ~ BB + singles + doubles + triples + HR, data = .)
coefs <- tidy(fit, conf.int = TRUE)
coefs

Teams %>% 
  filter(yearID %in% 2002) %>% 
  mutate(BB = BB/G, 
         singles = (H-X2B-X3B-HR)/G, 
         doubles = X2B/G, 
         triples =X3B/G, 
         HR=HR/G,
         R=R/G)  %>% 
  mutate(R_hat = predict(fit, newdata = .)) %>%
  ggplot(aes(R_hat, R, label = teamID)) + 
  geom_point() +
  geom_text(nudge_x=0.1, cex = 2) + 
  geom_abline()

pa_per_game <- Batting %>% filter(yearID == 2002) %>% 
  group_by(teamID) %>%
  summarize(pa_per_game = sum(AB+BB)/max(G)) %>% 
  pull(pa_per_game) %>% 
  mean

pa_per_game
players <- Batting %>% filter(yearID %in% 1999:2001) %>% 
  group_by(playerID) %>%
  mutate(PA = BB + AB) %>%
  summarize(G = sum(PA)/pa_per_game,
            BB = sum(BB)/G,
            singles = sum(H-X2B-X3B-HR)/G,
            doubles = sum(X2B)/G, 
            triples = sum(X3B)/G, 
            HR = sum(HR)/G,
            AVG = sum(H)/sum(AB),
            PA = sum(PA)) %>%
  filter(PA >= 300) %>%
  select(-G) %>%
  mutate(R_hat = predict(fit, newdata = .))

players
qplot(R_hat, data = players, geom = "histogram", binwidth = 0.5, color = I("black"))


view(Salaries)
players <- Salaries %>% 
  filter(yearID == 2002) %>%
  select(playerID, salary) %>%
  right_join(players, by="playerID")


position_names <- 
  paste0("G_", c("p","c","1b","2b","3b","ss","lf","cf","rf", "dh"))

view(Appearances)
tmp <- Appearances %>% 
  filter(yearID == 2002) %>% 
  group_by(playerID) %>%
  summarize_at(position_names, sum) %>%
  ungroup()
tmp

pos <- tmp %>%
  select(position_names) %>%
  apply(., 1, which.max) 
pos


players <- tibble(playerID = tmp$playerID, POS = position_names[pos]) %>%
  mutate(POS = str_to_upper(str_remove(POS, "G_"))) %>%
  filter(POS != "P") %>%
  right_join(players, by="playerID") %>%
  filter(!is.na(POS)  & !is.na(salary))

players

players <- Master %>%
  select(playerID, nameFirst, nameLast, debut) %>%
  mutate(debut = as.Date(debut)) %>%
  right_join(players, by="playerID")


library(Lahman)
library(tidyverse)
playerInfo <- Fielding %>%
  group_by(playerID) %>%
  arrange(desc(G)) %>%
  slice(1) %>%
  ungroup %>%
  left_join(Master, by="playerID") %>%
  select(playerID, nameFirst, nameLast, POS)

playerInfo

ROY <- AwardsPlayers %>%
  filter(awardID == "Rookie of the Year") %>%
  left_join(playerInfo, by="playerID") %>%
  rename(rookie_year = yearID) %>%
  right_join(Batting, by="playerID") %>%
  mutate(AVG = H/AB) %>%
  filter(POS != "P")
ROY

ROY <- ROY %>%
  filter(yearID == rookie_year | yearID == rookie_year+1) %>%
  group_by(playerID) %>%
  mutate(rookie = ifelse(yearID == min(yearID), "rookie", "sophomore")) %>%
  filter(n() == 2) %>%
  ungroup %>%
  select(playerID, rookie_year, rookie, nameFirst, nameLast, AVG)

ROY
ROY <- ROY %>% spread(rookie, AVG) %>% arrange(desc(rookie))
ROY

mean(ROY$sophomore - ROY$rookie <= 0)

two_years <- Batting %>%
  filter(yearID %in% 2013:2014) %>%
  group_by(playerID, yearID) %>%
  filter(sum(AB) >= 130) %>%
  summarize(AVG = sum(H)/sum(AB)) %>%
  ungroup %>%
  spread(yearID, AVG) %>%
  filter(!is.na(`2013`) & !is.na(`2014`)) %>%
  left_join(playerInfo, by="playerID") %>%
  filter(POS!="P") %>%
  select(-POS) %>%
  arrange(desc(`2013`)) %>%
  select(nameFirst, nameLast, `2013`, `2014`)
two_years

arrange(two_years, `2013`)

qplot(`2013`, `2014`, data = two_years)

summarize(two_years, cor(`2013`,`2014`))


library(dslabs)
library(tidyverse)
library(broom)

falling_object <- rfalling_object()

falling_object %>% 
  ggplot(aes(time, observed_distance)) + 
  geom_point() +
  ylab("Distance in meters") + 
  xlab("Time in seconds")

fit <- falling_object %>% 
  mutate(time_sq = time^2) %>% 
  lm(observed_distance~time+time_sq, data=.)
tidy(fit)
  
augment(fit) %>% 
  ggplot() +
  geom_point(aes(time, observed_distance)) + 
  geom_line(aes(time, .fitted), col = "blue")


fit <- Teams %>% 
  filter(yearID %in% 1971) %>% 
  mutate(BB = BB/G, HR = HR/G,  R = R/G) %>%  
  lm(R ~ BB + HR, data = .)
tidy(fit, conf.int = TRUE)

Teams %>% 
  filter(yearID %in% 1961:2018) %>% 
  mutate(BB = BB/G, HR = HR/G,  R = R/G) %>%  
  do(tidy(lm(R ~ BB + HR, data = .), conf.int = T))


bbfit = Teams %>% 
  filter(yearID %in% 1961:2018) %>% 
  mutate(BB = BB/G, HR = HR/G,  R = R/G) %>%  
  do(tidy(lm(R ~ BB + HR, data = .), conf.int = T)) %>%
  filter(term == "BB" ) %>%
  select(term, estimate, conf.low, conf.high) 

Teams %>% 
  filter(yearID %in% 1961:2018) %>% 
  group_by(yearID) %>%
  mutate(BB = BB/G, HR = HR/G,  R = R/G) %>%  
  do(tidy(lm(R ~ BB + HR, data = .), conf.int = T)) %>%
  filter(term == "BB" ) %>%
  select(yearID,term, estimate, conf.low, conf.high) %>% 
  ggplot(aes(yearID, y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_point()

bbfit = Teams %>% 
  filter(yearID %in% 1961:1965) %>% 
  group_by(yearID) %>%
  mutate(BB = BB/G, HR = HR/G,  R = R/G) %>%  
  do(tidy(lm(R ~ BB + HR, data = .), conf.int = T)) %>%
  filter(term == "BB" ) %>%
  select(yearID,estimate) 
bbfit

bbfit =  bbfit%>%
  mutate(yearID = yearID - 1961) %>%
  rename(es = estimate)

bbfit

bbfit %>%
do(tidy(lm(es ~ yearID, data = .), conf.int = T))

Teams %>% 
  filter(yearID %in% 1961:2018) %>% 
  mutate(BB = BB/G, HR = HR/G,  R = R/G) %>%  
  do(tidy(lm(R ~ BB + HR, data = .), conf.int = T))  

Teams %>% 
  filter(yearID %in% 1961:2018) %>%
  mutate(BB = BB/G, HR = HR/G,  R = R/G) %>%
  select(yearID,BB,HR,R)

fit = Teams %>% 
  filter(yearID %in% 1961:2018) %>% 
  group_by(yearID)  %>% 
  mutate(BB = BB/G, HR = HR/G,  R = R/G) %>%  
  do(tidy(lm(R ~ BB + HR, data = .), conf.int = T)) %>%
  mutate(yearID = yearID - 1960) %>%
  filter(term == "BB" ) %>%
  select(yearID,estimate) 

fit

Teams %>% 
  filter(yearID %in% 1961:2018) %>% 
  group_by(yearID) %>% 
  mutate(BB = BB/G, HR = HR/G, R = R/G) %>% 
  do(tidy(lm(R ~ BB + HR, data = .))) %>% 
  filter(term == "BB") %>% 
  lm(estimate ~ yearID, data = .) %>% 
  tidy()

fit = Teams %>% 
  filter(yearID %in% 1961:2018) %>% 
  group_by(yearID)  %>% 
  mutate(BB = BB, HR = HR,  R = R) %>%  
  do(tidy(lm(R ~ BB + HR, data = .), conf.int = T)) %>%
  mutate(yearID = yearID - 1960) %>%
  filter(term == "BB" ) 

fit %>%
  lm(estimate ~ yearID, data = .) %>% 
  tidy()
