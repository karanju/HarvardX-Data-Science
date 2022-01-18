library(tidyverse)
library(dslabs)
data(heights)

# compute average and standard deviation for males
s <- heights %>%
  filter(sex == "Male") %>%
  summarize(average = mean(height), standard_deviation = sd(height))

# access average and standard deviation from summary table
s$average
s$standard_deviation

heights %>%
  filter(sex == "Male") %>%
  summarize(median = median(height),
            minimum = min(height),
            maximum = max(height))
# alternative way to get min, median, max in base R
quantile(heights$height, c(0, 0.5, 1))


us_murder_rate <- murders %>%
  summarize(rate = sum(total) / sum(population) * 100000)
us_murder_rate

us_murder_rate %>% .$rate

us_murder_rate <- murders %>%
  summarize(rate = sum(total) / sum(population) * 100000) %>%
  .$rate


heights %>%
  group_by(sex) %>%
  summarize(average = mean(height), standard_deviation = sd(height))

murders <- murders %>%
  mutate(murder_rate = total/population * 100000)
murders %>%
  group_by(region) %>%
  summarize(median_rate = median(murder_rate))


murders %>% arrange(population) %>% head()
murders %>% arrange(desc(murder_rate)) %>% head()
murders %>% arrange(region, murder_rate) %>% head()
murders %>% arrange(desc(murder_rate)) %>% top_n(10)


library(dslabs)
data(na_example)
mean(na_example)
sd(na_example)
mean(na_example, na.rm = TRUE)
sd(na_example, na.rm = TRUE)

library(dplyr)
library(NHANES)
data(NHANES)

tab <- NHANES  %>%  filter(Gender == "female") %>% filter(AgeDecade == " 20-29") %>% head()
length(tab)

tab <- NHANES  %>%  filter(Gender == "female" & AgeDecade == " 20-29")  %>% head()
length(tab)


a = (NHANES$AgeDecade==" 20-29")
sum(a,na.rm=TRUE)
x


a = head(NHANES)
a
