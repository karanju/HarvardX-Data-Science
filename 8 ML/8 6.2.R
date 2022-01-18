library(tidyverse)
library(dslabs)
data("movielens")

movielens %>% as_tibble()

movielens %>% 
  summarize(n_users = n_distinct(userId),
            n_movies = n_distinct(movieId))

keep <- movielens %>%
  dplyr::count(movieId) %>%
  top_n(5) %>%
  pull(movieId)
tab <- movielens %>%
  filter(userId %in% c(13:20)) %>% 
  filter(movieId %in% keep) %>% 
  select(userId, title, rating) %>% 
  spread(title, rating)
tab %>% knitr::kable()


users <- sample(unique(movielens$userId), 100)
rafalib::mypar()
movielens %>% filter(userId %in% users) %>% 
  select(userId, movieId, rating) %>%
  mutate(rating = 1) %>%
  spread(movieId, rating) %>% select(sample(ncol(.), 100)) %>% 
  as.matrix() %>% t(.) %>%
  image(1:100, 1:100,. , xlab="Movies", ylab="Users")
abline(h=0:100+0.5, v=0:100+0.5, col = "grey")

movielens %>% 
  dplyr::count(movieId) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins = 30, color = "black") + 
  scale_x_log10() + 
  ggtitle("Movies")

movielens %>%
  dplyr::count(userId) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins = 30, color = "black") + 
  scale_x_log10() +
  ggtitle("Users")


library(caret)
set.seed(755)
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.2, 
                                  list = FALSE)
train_set <- movielens[-test_index,]
test_set <- movielens[test_index,]

test_set <- test_set %>% 
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")

RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

view(test_set)


mu_hat <- mean(train_set$rating)
mu_hat

naive_rmse <- RMSE(test_set$rating, mu_hat)
naive_rmse

predictions <- rep(3, nrow(test_set))
predictions
RMSE(test_set$rating, predictions)


rmse_results <- data_frame(method = "Just the average", RMSE = naive_rmse)

#fit <- lm(rating ~ as.factor(userId), data = movielens)
mu <- mean(train_set$rating) 
movie_avgs <- train_set %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu))

qplot(b_i, data = movie_avgs, bins = 10, color = I("black"))

predicted_ratings <- mu + test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  .$b_i

model_1_rmse <- RMSE(predicted_ratings, test_set$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie Effect Model",
                                     RMSE = model_1_rmse ))

rmse_results %>% knitr::kable()

train_set %>% 
  group_by(userId) %>% 
  summarize(b_u = mean(rating)) %>% 
  filter(n()>=100) %>%
  ggplot(aes(b_u)) + 
  geom_histogram(bins = 30, color = "black")

# lm(rating ~ as.factor(movieId) + as.factor(userId))
user_avgs <- test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))


predicted_ratings <- test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  .$pred

model_2_rmse <- RMSE(predicted_ratings, test_set$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie + User Effects Model",  
                                     RMSE = model_2_rmse ))
rmse_results %>% knitr::kable()



library(tidyverse)
library(lubridate)
library(dslabs)
data("movielens")

view(movielens)

movielens %>%
  dplyr::count(movieId) %>% mutate(y = year)

m = movielens %>% group_by(movieId) %>%
  summarize(n = n(), year = as.character(first(year))) 

mx = max(m$n)
nrow(movielens[movielens$movieId==356,])
a = movielens[movielens$movieId==356,]
sum(a$rating)

m %>% filter(year>1990) %>% ggplot(aes(x=year,y=n)) + geom_boxplot() + scale_y_sqrt()



m = movielens %>% filter(year>=1993) %>% mutate(date =  format(as.Date(as.POSIXct(timestamp, origin="1970-01-01"),format="%d/%m/%Y"),"%Y"))
view(m)
avgs = m %>% group_by(movieId) %>% mutate(n=n(),first=2018 - as.numeric(first(year))) %>% mutate(avg=n/first) 
avgs
avgs = avgs %>% group_by(movieId) %>% summarise(avgrat = sum(rating)/n(),title=as.character(first(title)),avg = first(avg))
avgs = avgs[order(-avgs$avg),]
view(avgs) 

avgs %>% ggplot(aes(avgrat,avg)) + geom_point()


movielens <- mutate(movielens, date = as_datetime(timestamp))

format(as.Date(as_datetime(1260759144) ,format="%d/%m/%Y"),"%Y")

m = movielens %>% mutate(date = round_date(movielens$date,unit="week",week_start = getOption("lubridate.week.start", 7)) )

m = m %>% group_by(date) %>% summarize(rating=mean(rating))

m
m %>% ggplot(aes(date,rating))+
  geom_point() +
  geom_smooth()

movielens %>% mutate(date = round_date(date, unit = "week")) %>%
  group_by(date) %>%
  summarize(rating = mean(rating)) %>%
  ggplot(aes(date, rating)) +
  geom_point() +
  geom_smooth()

a = movielens[movielens$title=="Die Hard",]
mean(a$rating,na.rm=TRUE)

m = movielens %>% group_by(genres) %>% summarise(n=n(), avg=mean(rating), sd= sd(rating)) %>% filter(n>1000)
view(m)

min(m$avg)

m %>% ggplot() +
  geom_bar( aes(x=genres, y=avg), stat="identity", fill="skyblue", alpha=0.7) +
  geom_errorbar( aes(x=genres, ymin=avg-sd, ymax=avg+sd), width=0.4, colour="orange", alpha=0.9, size=1.3) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
m %>% ggplot() + geom_bar( aes(x=genres, y=avg))
m
