options(digits = 3)    # report 3 significant digits
library(tidyverse)
library(titanic)

titanic <- titanic_train %>%
  select(Survived, Pclass, Sex, Age, SibSp, Parch, Fare) %>%
  mutate(Survived = factor(Survived),
         Pclass = factor(Pclass),
         Sex = factor(Sex))

?titanic_train
titanic
levels(titanic$Fare)

titanic %>%
  ggplot(aes(Age,group=Sex, fill=Sex, alpha=0.5)) + geom_density()

sum(titanic$Sex=="female")
sum(titanic$Sex=="male")

sum(titanic$Sex=="female" & titanic$Age>40.00, na.rm=TRUE)
sum(titanic$Sex=="male" & titanic$Age>40.00 , na.rm=TRUE)

mean(titanic$Sex=="female" & titanic$Age>18.00 & titanic$Age<35, na.rm=TRUE)
mean(titanic$Sex=="male" & titanic$Age>18.00 & titanic$Age<35 , na.rm=TRUE)

mean(titanic$Sex=="female" & titanic$Age<17.00, na.rm=TRUE)
mean(titanic$Sex=="male" & titanic$Age<17.00 , na.rm=TRUE)

params <- titanic %>%
  filter(!is.na(Age)) %>%
  summarize(mean = mean(Age), sd = sd(Age))

titanic %>%
  ggplot() + geom_qq(aes(sample=Age),dparams = params) + geom_abline()


titanic %>%
  ggplot(aes(Survived ,fill=Sex)) + geom_bar()


titanic %>%
  ggplot(aes(Survived ,fill=Sex)) + geom_bar(position = position_dodge())

titanic %>%
  ggplot(aes(Age, fill=Survived, alpha=0.2)) + geom_density()

titanic %>%
  filter(Fare != 0.00 ) %>%
  ggplot(aes(Fare)) + geom_boxplot() +
  scale_x_continuous(trans = "log2") 

titanic %>%
  filter(Fare != 0.00 ) %>%
  ggplot(aes(x =Survived, y=Fare, group=Survived, color=Survived)) + geom_boxplot() +
    scale_y_continuous(trans = "log2") + geom_point() + geom_jitter()


titanic %>%
  ggplot(aes(Pclass ,fill=Survived)) + geom_bar()

titanic %>%
  ggplot(aes(Pclass ,fill=Survived)) + geom_bar(position = position_fill() )


titanic %>%
  ggplot(aes(Survived ,fill=Pclass)) + geom_bar(position = position_fill() )

titanic %>%
  ggplot(aes(x=Age, fill=Survived, alpha=0.2)) + geom_density() +
  facet_grid(Sex ~ Pclass)

titanic %>%
  ggplot(aes(x=Age,y = ..count.., fill=Survived, alpha=0.2)) + geom_density() +
  facet_grid(Sex ~ Pclass)
