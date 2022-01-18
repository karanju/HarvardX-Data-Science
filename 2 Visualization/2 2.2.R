library(dslabs)
data(murders)
head(murders)
sum( (murders$total/murders$population) > 10/100000 )
p <- seq(0.01, 0.99, 0.01)
quantile(murders$population, p)
library(dslabs)
data(heights)
summary(heights$height)
p <- seq(0.01, 0.99, 0.01)
quantile(heights$height, p)
percentiles <- quantile(heights$height, p)
percentiles[names(percentiles)=="25%"]


library(tidyverse)
library(dslabs)
data(murders)

ggplot(data = murders)

p <- ggplot(data = murders)
class(p)
print(p)    # this is equivalent to simply typing p
p
p + geom_point(aes(population/10^6, total))
p + geom_point(aes(population/10^6, total)) +
  geom_text(aes(population/10^6, total, label = abb))
pop = murders$population/10^6
p + geom_text(aes(x = pop,y=total,label=abb))

p + geom_point(aes(population/10^6, total), size = 3) +
  geom_text(aes(population/10^6, total, label = abb))

p + geom_point(aes(population/10^6, total), size = 3) +
  geom_text(aes(population/10^6, total, label = abb), nudge_x = 1.5)

p <- murders %>% ggplot(aes(population/10^6, total, label = abb))
p + geom_point(size = 1) +
  geom_text(nudge_x = 1.5)

p <- murders %>% ggplot(aes(pop, total, label = abb))
p
p + geom_point(size = 3) +
  geom_text(nudge_x = 0.05) +
  scale_x_continuous(trans = "log10") +
  scale_y_continuous(trans = "log10")

p + geom_point(size = 3) +
  geom_text(nudge_x = 0.075) +
  scale_x_log10() +
  scale_y_log10()

p + geom_point(size = 3) +
  geom_text(nudge_x = 0.075) +
  scale_x_log10() +
  scale_y_log10() +
  xlab("Population in millions (log scale)") +
  ylab("Total number of murders (log scale)") +
  ggtitle("US Gun Murders in 2010")
p + geom_point(size = 3, color = "blue")
p + geom_point(aes(col = region), size = 3)

r = mean(murders$total/murders$population)*10^6

p + geom_point(aes(col = region), size = 3) +
  geom_abline(intercept = log10(r)) 

p + 
  geom_abline(intercept = log10(r), lty = 2, color = "darkgrey") +
  geom_point(aes(col = region), size = 3)
r

library(ggrepel)
library(ggthemes)

r <- murders %>%
  summarize(rate = sum(total) / sum(population) * 10^6) %>%
  .$rate
r
murders %>%
  ggplot(aes(population/10^6, total, label = abb)) +
  geom_abline(intercept = log10(r), lty = 2, color = "darkgrey") +
  geom_point(aes(col = region), size = 3) +
  geom_text_repel() +
  scale_x_log10() +
  scale_y_log10() +
  xlab("Population in millions (log scale)") +
  ylab("Total number of murders (log scale)") +
  ggtitle("US Gun Murders in 2010") +
  scale_color_discrete(name = "Region") +
  theme_economist()


p <- heights %>%
  filter(sex == "Male") %>%
  ggplot(aes(x = height))

# basic histograms
p + geom_histogram()
p + geom_histogram(binwidth = 1)
p + geom_histogram(binwidth = 1, fill = "blue", col = "black") +
  xlab("Male heights in inches") +
  ggtitle("Histogram")

p <- heights %>% filter(sex == "Male") %>%
  ggplot(aes(sample = height))
p + geom_qq()

params <- heights %>%
  filter(sex == "Male") %>%
  summarize(mean = mean(height), sd = sd(height))
p + geom_qq(dparams = params) +
  geom_abline()

heights %>%
  ggplot(aes(sample = scale(height))) +
  geom_qq() +
  geom_abline()


p <- ggplot(murders)
p
