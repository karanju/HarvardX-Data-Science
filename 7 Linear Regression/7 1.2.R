library(tidyverse)
library(HistData)
data("GaltonFamilies")

set.seed(1983)
galton_heights <- GaltonFamilies %>%
  filter(gender == "male") %>%
  group_by(family) %>%
  sample_n(1) %>%
  ungroup() %>%
  select(father, childHeight) %>%
  rename(son = childHeight)

galton_heights %>%
  summarize(mean(father), sd(father), mean(son), sd(son))

galton_heights %>%
  ggplot(aes(father, son)) +
  geom_point(alpha = 0.5)


galton_heights %>% summarize(r = cor(father, son)) %>% pull(r)

R <- sample_n(galton_heights, 25, replace = TRUE) %>%
  summarize(r = cor(father, son))
R

B <- 1000
N <- 25
R <- replicate(B, {
  sample_n(galton_heights, N, replace = TRUE) %>%
    summarize(r = cor(father, son)) %>%
    pull(r)
})
qplot(R, geom = "histogram", binwidth = 0.05, color = I("black"))

mean(R)
sd(R)

data.frame(R) %>%
  ggplot(aes(sample = R)) +
  stat_qq() +
  geom_abline(intercept = mean(R), slope = sqrt((1-mean(R)^2)/(N-2)))


B <- 1000
N <- 50
R <- replicate(B, {
  sample_n(galton_heights, N, replace = TRUE) %>%
    summarize(r = cor(father, son)) %>%
    pull(r)
})
qplot(R, geom = "histogram", binwidth = 0.05, color = I("black"))

mean(R)
sd(R)

library(Lahman)
library(tidyverse)
library(dslabs)
ds_theme_set()

Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(AB_per_game = AB / G, R_per_game = R / G) %>%
  summarize(r = cor(AB_per_game, R_per_game))

Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(W_per_game = W / G, E_per_game = E / G) %>%
  summarize(r = cor(W_per_game, E_per_game))

Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(X2B_per_game = X2B / G, X3B_per_game = X3B / G) %>%
  summarize(r = cor(X2B_per_game, X3B_per_game))
