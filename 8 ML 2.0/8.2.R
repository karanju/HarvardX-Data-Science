library(tidyverse)
library(caret)
library(dslabs)
data(heights)

y <- heights$sex
x <- heights$height

set.seed(2, sample.kind = "Rounding") # if using R 3.5 or earlier, remove the sample.kind argument
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)

test_set <- heights[test_index, ]
train_set <- heights[-test_index, ]

y_hat <- sample(c("Male", "Female"), length(test_index), replace = TRUE) %>%
  factor(levels = levels(test_set$sex))

mean(y_hat == test_set$sex)

heights %>% group_by(sex) %>% summarize(mean(height), sd(height))

y_hat <- ifelse(x > 62, "Male", "Female") %>% 
  factor(levels = levels(test_set$sex))

mean(y == y_hat)

cutoff <- seq(61, 70)
accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train_set$height > x, "Male", "Female") %>% 
    factor(levels = levels(test_set$sex))
  mean(y_hat == train_set$sex)
})

data.frame(cutoff, accuracy) %>% 
  ggplot(aes(cutoff, accuracy)) + 
  geom_point() + 
  geom_line() 
max(accuracy)

best_cutoff <- cutoff[which.max(accuracy)]
best_cutoff

y_hat <- ifelse(test_set$height > best_cutoff, "Male", "Female") %>% 
  factor(levels = levels(test_set$sex))
y_hat <- factor(y_hat)
mean(y_hat == test_set$sex)

mnist <- read_mnist() 
dim(mnist$train$images)

cutoff <- seq(61, 70)
F_1 <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train_set$height > x, "Male", "Female") %>% 
    factor(levels = levels(test_set$sex))
  F_meas(data = y_hat, reference = factor(train_set$sex))
})

data.frame(cutoff, F_1) %>% 
  ggplot(aes(cutoff, F_1)) + 
  geom_point() + 
  geom_line()

max(F_1)

best_cutoff <- cutoff[which.max(F_1)]
best_cutoff


y_hat <- ifelse(test_set$height > best_cutoff, "Male", "Female") %>% 
  factor(levels = levels(test_set$sex))
sensitivity(data = y_hat, reference = test_set$sex)
specificity(data = y_hat, reference = test_set$sex)


library(dslabs)
library(dplyr)
library(lubridate)
data(reported_heights)

dat <- mutate(reported_heights, date_time = ymd_hms(time_stamp)) %>%
  filter(date_time >= make_date(2016, 01, 25) & date_time < make_date(2016, 02, 1)) %>%
  mutate(type = ifelse(day(date_time) == 25 & hour(date_time) == 8 & between(minute(date_time), 15, 30), "inclass","online")) %>%
  select(sex, type)

y <- factor(dat$sex, c("Female", "Male"))
x <- dat$type
dat$sex
dat %>% group_by(type) %>% summarise(mean(sex=='Female'))

y_hat <- ifelse(x=='online', "Male", "Female") %>% 
  factor(levels = levels(test_set$sex))
mean(y==y_hat)

table(y_hat, y)
sensitivity(data = y_hat, reference = y)
specificity(data = y_hat, reference = y)
mean(y=='Female')

library(caret)
data(iris)
iris <- iris[-which(iris$Species=='setosa'),]
y <- iris$Species
y

# set.seed(2) # if using R 3.5 or earlier
set.seed(2, sample.kind="Rounding") # if using R 3.6 or later
test_index <- createDataPartition(y,times=1,p=0.5,list=FALSE)
test <- iris[test_index,]
train <- iris[-test_index,]
train




cutoff <- seq( min(test$Sepal.Length, na.rm=TRUE), max(test$Sepal.Length, na.rm=TRUE) , by=0.1)
accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(test$Sepal.Length > x, "virginica", "versicolor") %>% 
    factor(levels = levels(y))
  mean(y_hat == train$Species)
})
max(accuracy)

cutoff <- seq( min(train$Petal.Width, na.rm=TRUE), max(train$Petal.Width, na.rm=TRUE) , by=0.1)
accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train$Petal.Width > x, "virginica", "versicolor") %>% 
    factor(levels = levels(y))
  mean(y_hat == train$Species)
})
max(accuracy)

cutoff <- seq( min(train$Petal.Length, na.rm=TRUE), max(train$Petal.Length, na.rm=TRUE) , by=0.1)
accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train$Petal.Length > x, "virginica", "versicolor") %>% 
    factor(levels = levels(y))
  mean(y_hat == train$Species)
})
max(accuracy)

best_cutoff <- cutoff[which.max(accuracy)]
best_cutoff

y_hat <- ifelse(test$Petal.Length > best_cutoff | test$Petal.Width > 1.5, "virginica", "versicolor") %>% 
  factor(levels = levels(y))
y_hat <- factor(y_hat)
mean(y_hat == test$Species)


library(dslabs)
data("heights")
heights %>% 
mutate(height = round(height)) %>%
group_by(height) %>%
summarize(p = mean(sex == "Male")) %>%
qplot(height, p, data =.)

ps <- seq(0, 1, 0.1)
heights %>% 
  mutate(g = cut(height, quantile(height, ps), include.lowest = TRUE)) %>%
  group_by(g) %>%
  summarize(p = mean(sex == "Male"), height = mean(height)) %>%
  qplot(height, p, data =.)


dataset <- c( 0,1,2, 3,4,5, 7,10,15 )       # note the uneven spread of data
x <- quantileCut( dataset, 3 )              # cut into 3 equally frequent bins
table(x)     
