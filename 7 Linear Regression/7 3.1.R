N <- 25
g <- 1000000
sim_data <- tibble(group = rep(1:g, each=N), 
                   x = rnorm(N * g), 
                   y = rnorm(N * g))

res <- sim_data %>% 
  group_by(group) %>% 
  summarize(r = cor(x, y)) %>% 
  arrange(desc(r))

res

sim_data %>% filter(group == res$group[which.max(res$r)]) %>%
  ggplot(aes(x, y)) +
  geom_point() + 
  geom_smooth(method = "lm")

res %>% ggplot(aes(x=r)) + geom_histogram(binwidth = 0.1, color = "black")


set.seed(1985)
x <- rnorm(100,100,1)
y <- rnorm(100,84,1)
x[-23] <- scale(x[-23])
y[-23] <- scale(y[-23])
x
y
qplot(x, y)
cor(x,y)
cor(x[-23], y[-23])
rank(x)
rank(y)
qplot(rank(x), rank(y))
cor(rank(x), rank(y))
cor(x, y, method = "spearman")


library(HistData)
data("GaltonFamilies")
GaltonFamilies %>%
  filter(childNum == 1 & gender == "male") %>%
  select(father, childHeight) %>%
  rename(son = childHeight) %>% 
  do(tidy(lm(father ~ son, data = .)))



data(admissions)
View(admissions)
admissions %>% group_by(gender) %>% 
  summarize(total_admitted = round(sum(admitted / 100 * applicants)), 
            not_admitted = sum(applicants) - sum(total_admitted)) %>%
  select(-gender) %>% 
  do(tidy(chisq.test(.))) %>% .$p.value

admissions %>% select(major, gender, admitted) %>%
  spread(gender, admitted) %>%
  mutate(women_minus_men = women - men)


admissions %>% 
  group_by(major) %>% 
  summarize(major_selectivity = sum(admitted * applicants)/sum(applicants),
            percent_women_applicants = sum(applicants * (gender=="women")) /
              sum(applicants) * 100) %>%
  ggplot(aes(major_selectivity, percent_women_applicants, label = major)) +
  geom_text()


admissions %>% 
  ggplot(aes(major, admitted, col = gender, size = applicants)) +
  geom_point()

admissions %>%  group_by(gender) %>% summarize(average = mean(admitted))








library(dslabs)
data("research_funding_rates")
research_funding_rates
view(research_funding_rates)

totals <- research_funding_rates %>% 
  select(-discipline) %>% 
  summarize_all(sum) %>%
  summarize(yes_men = awards_men, 
            no_men = applications_men - awards_men, 
            yes_women = awards_women, 
            no_women = applications_women - awards_women)

totals$yes_men/ (totals$yes_men + totals$no_men) 
totals$yes_women/ (totals$no_women + totals$yes_women) 

totals %>%
  do(tidy(chisq.test(.))) %>% .$p.value

two_by_two <- data.frame(awarded = c("no", "yes"), 
                         men = c(totals$no_men, totals$yes_men),
                         women = c(totals$no_women, totals$yes_women))
two_by_two

data.frame(awarded = c("no", "yes"), 
           men = (totals$no_men + totals$yes_men) * c(1 - rate, rate),
           women = (totals$no_women + totals$yes_women) * c(1 - rate, rate))

chisq_test <- two_by_two %>% select(-awarded) %>% chisq.test()
chisq_test$p.value

dat <- research_funding_rates %>% 
  mutate(discipline = reorder(discipline, success_rates_total)) %>%
  rename(success_total = success_rates_total,
         success_men = success_rates_men,
         success_women = success_rates_women) %>%
  gather(key, value, -discipline) %>%
  separate(key, c("type", "gender")) %>%
  spread(type, value) %>%
  filter(gender != "total")
dat

dat %>% 
  ggplot(aes(discipline , success, col = gender, size = applications )) +
  geom_point() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

research_funding_rates %>% select(discipline,success_rates_total) 
