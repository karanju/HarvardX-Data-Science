library(titanic)    # loads titanic_train data frame
library(caret)
library(tidyverse)
library(rpart)

# 3 significant digits
options(digits = 3)
options

# clean the data - `titanic_train` is loaded with the titanic package
titanic_clean <- titanic_train %>%
  mutate(Survived = factor(Survived),
         Embarked = factor(Embarked),
         Age = ifelse(is.na(Age), median(Age, na.rm = TRUE), Age), # NA age to median age
         FamilySize = SibSp + Parch + 1) %>%    # count family members
  select(Survived,  Sex, Pclass, Age, Fare, SibSp, Parch, FamilySize, Embarked)

titanic_clean

set.seed(42, sample.kind="Rounding")
test_index <- createDataPartition(titanic_clean$Survived, times = 1, p = 0.2, list = FALSE)
test_index
test_set <- titanic_clean[test_index, ]
train_set <- titanic_clean[-test_index, ]     
n=length(test_set[,1])
length(train_set[,1])

mean(train_set$Survived==1)

set.seed(3, sample.kind="Rounding")
guess = sample(c(1,0),size=n,prob=c(0.5,0.5),replace=TRUE) %>% factor()
mean(test_set$Survived==guess)

sum(train_set$Sex=="female" & train_set$Survived==1)/sum(train_set$Sex=="female")
sum(train_set$Sex=="male" & train_set$Survived==1)/sum(train_set$Sex=="male")

guess_x = ifelse(test_set$Sex == "female",1,0) %>% factor()
mean(test_set$Survived == guess_x)


sum(train_set$Pclass==1 & train_set$Survived==1)/sum(train_set$Pclass==1)
sum(train_set$Pclass==2 & train_set$Survived==1)/sum(train_set$Pclass==2)
sum(train_set$Pclass==3 & train_set$Survived==1)/sum(train_set$Pclass==3)

guess_c = ifelse(test_set$Pclass==1,1,0) %>% factor()
mean(test_set$Survived == guess_c)


sum(train_set$Pclass==1 & train_set$Sex=="male" & train_set$Survived==1)/sum(train_set$Pclass==1 & train_set$Sex=="male")
sum(train_set$Pclass==2 & train_set$Sex=="female" & train_set$Survived==1)/sum(train_set$Pclass==2 & train_set$Sex=="female" )
sum(train_set$Pclass==3 & train_set$Sex=="female" & train_set$Survived==1)/sum(train_set$Pclass==3 & train_set$Sex=="female" )


guess_xc = ifelse((test_set$Pclass==1 | test_set$Pclass==2 ) & test_set$Sex=="female",1,0) %>% factor()
mean(guess_xc)
mean(test_set$Survived == guess_xc)

confusionMatrix(guess_x, test_set$Survived)
confusionMatrix(guess_c, test_set$Survived)
confusionMatrix(guess_xc, test_set$Survived)

F_meas(data = guess_x, reference = factor(test_set$Survived)) 
F_meas(data = guess_c, reference = factor(test_set$Survived)) 
F_meas(data = guess_xc, reference = factor(test_set$Survived)) 

library(caret)
train_lda <- train(Survived ~ Fare , method = "lda", data = train_set)
y_hat_lda <- predict(train_lda, test_set, type = "raw")
confusionMatrix(y_hat_lda, test_set$Survived)$overall[["Accuracy"]]

train_qda <- train(Survived ~ Fare , method = "qda", data = train_set)
y_hat_qda <- predict(train_qda, test_set, type = "raw")
confusionMatrix(y_hat_qda, test_set$Survived)$overall[["Accuracy"]]       
       
train_set
set.seed(1, sample.kind="Rounding")
train_glm1 <- train(Survived ~ Age , method = "glm", data = train_set)
y_hat_glm1 <- predict(train_glm1, test_set, type = "raw")
confusionMatrix(y_hat_glm1, test_set$Survived)$overall[["Accuracy"]] 

set.seed(1, sample.kind="Rounding")
train_glm2 <- train(Survived ~ Sex + Pclass + Fare + Age , method = "glm", data = train_set)
y_hat_glm2 <- predict(train_glm2, test_set, type = "raw")
confusionMatrix(y_hat_glm2, test_set$Survived)$overall[["Accuracy"]]

set.seed(1, sample.kind="Rounding")
train_glm3 <- train(Survived ~ . , method = "glm", data = train_set)
y_hat_glm3 <- predict(train_glm3, test_set, type = "raw")
confusionMatrix(y_hat_glm3, test_set$Survived)$overall[["Accuracy"]]


set.seed(6, sample.kind="Rounding")
train_knn <- train(Survived ~ . , method = "knn", tuneGrid = data.frame(k = seq(3, 51, 2)), data = train_set)
train_knn$bestTune
y_hat_knn <- predict(train_knn, test_set, type = "raw")
confusionMatrix(y_hat_knn, test_set$Survived)$overall[["Accuracy"]]


set.seed(8, sample.kind="Rounding")
train_knn <- train(Survived ~ . , method = "knn", tuneGrid = data.frame(k = seq(3, 51, 2)), data = train_set)
train_knn$bestTune
y_hat_knn <- predict(train_knn, test_set, type = "raw")
confusionMatrix(y_hat_knn, test_set$Survived)$overall[["Accuracy"]]

set.seed(8, sample.kind="Rounding")
control <- trainControl(method = "cv", number = 10, p = .9)
train_knn <- train(Survived ~ . , method = "knn", tuneGrid = data.frame(k = seq(3, 51, 2)), trControl = control,data = train_set)
train_knn$bestTune
plot(train_knn)
y_hat_knn <- predict(train_knn, test_set, type = "raw")
confusionMatrix(y_hat_knn, test_set$Survived)$overall[["Accuracy"]]

set.seed(10, sample.kind="Rounding")
train_rpart <- train(Survived ~ . , method = "rpart", tuneGrid = data.frame(cp = seq(0, 0.05, 0.002)), data = train_set)
ggplot(train_rpart, highlight = TRUE)
train_rpart$bestTune
y_hat_rp <- predict(train_rpart, test_set, type = "raw")
confusionMatrix(y_hat_rp, test_set$Survived)$overall[["Accuracy"]]
plot(train_rpart$finalModel, margin = 0.1)
text(train_rpart$finalModel, cex = 0.75)

y_hat_rp <- predict(train_rpart, test_set, type = "raw")
test_set
y_hat_rp


set.seed(14, sample.kind="Rounding")
train_rpart <- train(Survived ~ . , method = "rf", tuneGrid = data.frame(mtry = seq(1,7)), ntree=100, data = train_set)
ggplot(train_rpart, highlight = TRUE)
train_rpart$bestTune
y_hat_rp <- predict(train_rpart, test_set, type = "raw")
confusionMatrix(y_hat_rp, test_set$Survived)$overall[["Accuracy"]]
imp = varImp(train_rpart)
imp
