library(tidyverse)
library(dslabs)
data(gapminder)

tidy_data <- gapminder %>% 
  filter(country %in% c("South Korea", "Germany")) %>%
  select(country, year, fertility)
head(tidy_data)

tidy_data %>% 
  ggplot(aes(year, fertility, color = country)) +
  geom_point()

path <- system.file("extdata", package="dslabs")
filename <- file.path(path,  "fertility-two-countries-example.csv")
wide_data <- read_csv(filename)
select(wide_data, country, `1960`:`1967`)

new_tidy_data <- gather(wide_data, year, fertility, `1960`:`2015`)
new_tidy_data
new_tidy_data <- wide_data %>% gather(year, fertility, `1960`:`2015`)
new_tidy_data <- wide_data %>%
  gather(year, fertility, -country)
new_tidy_data <- wide_data %>%
  gather(year, fertility, -country, convert = TRUE)
class(new_tidy_data$year)

new_wide_data <- new_tidy_data %>% spread(year, fertility)
select(new_wide_data, country, `1960`:`1967`)


path <- system.file("extdata", package = "dslabs")

filename <- "life-expectancy-and-fertility-two-countries-example.csv"
filename <-  file.path(path, filename)

raw_dat <- read_csv(filename)
select(raw_dat, 1:5)

dat <- raw_dat %>% gather(key, value, -country)
head(dat)
dat %>% separate(key, c("year", "variable_name"), "_")

var_names <- c("year", "first_variable_name", "second_variable_name")
dat %>% separate(key, var_names, fill = "right")

dat %>% separate(key, c("year", "variable_name"), extra = "merge")

dat %>% 
  separate(key, c("year", "variable_name"), extra = "merge") %>%
  spread(variable_name, value) 


library(tidyverse)
library(dslabs)
view(co2)
head(co2)

co2_wide <- data.frame(matrix(co2, ncol = 12, byrow = TRUE)) %>% 
  setNames(1:12) %>%
  mutate(year = as.character(1959:1997))
head(co2_wide)
co2_tidy <- gather(co2_wide,month,co2,-year)

head(co2_tidy)

co2_tidy %>% ggplot(aes(as.numeric(month), co2, color = year)) + geom_line()

data(admissions)
head(admissions)
dat <- admissions %>% select(-applicants)
head(dat)

tmp <- gather(admissions, key, value, admitted:applicants)
head(tmp)
