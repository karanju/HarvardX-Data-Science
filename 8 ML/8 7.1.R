options(digits = 3)
library(matrixStats)
library(tidyverse)
library(caret)
library(dslabs)
data(brca)

y = brca$y
x = brca$x

length(y)
dim(x)
y
mean(y=="M")
cm = colMeans(x)
which.max(cm)
cs = colSds(x)
which.min(cs)

x = sweep(x, 2, colMeans(x, na.rm=TRUE))
x = sweep(x, 2, colSds(x, na.rm=TRUE), FUN = "/")
sd(x[,1])
median(x[,1])

d <- dist(x)
as.matrix(d)
dim(as.matrix(d))
b = y=="B"
mean(as.matrix(d)[1,b])
b = y=="M"
mean(as.matrix(d)[1,b])


d_features <- dist(t(x))
heatmap(as.matrix(d_features), labRow = NA, labCol = NA)

hc <- hclust(d_features, method = "complete")              
hc
plot(hc, cex = 0.6, hang = -1)
hc1 = cutree(hc, k = 5)
plot(hc1, cex = 0.6, hang = -1)

pca <- prcomp(x)
summary(pca)
pca
view(pca)
dim(pca$rotation)
pca$rotation
dim(pca$x)
pca$x

qplot(1:30, pca$sdev, xlab = "PC")


var_explained <- cumsum(pca$sdev^2/sum(pca$sdev^2))
var_explained
plot(var_explained)

plot(pca$x[,1], pca$x[,2],col=y)

data.frame(pca$x[,1:2], type = brca$y) %>%
  ggplot(aes(PC1, PC2, color = type)) +
  geom_point()


data.frame(pca$x[,1:10], type = brca$y) %>%
  ggplot(aes(PC10, color = type)) +
  geom_boxplot()

data.frame(type = brca$y, pca$x[,1:10]) %>%
  gather(key = "PC", value = "value", -type) %>%
  ggplot(aes(PC, value, fill = type)) +
  geom_boxplot()


set.seed(1) # if using R 3.5 or earlier
set.seed(1, sample.kind = "Rounding")    # if using R 3.6 or later
test_index <- createDataPartition(brca$y, times = 1, p = 0.2, list = FALSE)
test_x <- x[test_index,]
test_y <- brca$y[test_index]
train_x <- x[-test_index,]
train_y <- brca$y[-test_index]

mean(test_y=="B")
mean(train_y=="B")

predict_kmeans <- function(x, k) {
  centers <- k$centers    # extract cluster centers
  # calculate distance to cluster centers
  distances <- sapply(1:nrow(x), function(i){
    apply(centers, 1, function(y) dist(rbind(x[i,], y)))
  })
  max.col(-t(distances))  # select cluster with min distance to center
}

set.seed(3, sample.kind = "Rounding")     
km <- kmeans(train_x, 2, nstart = 25)
km
y_hat_km = predict_kmeans(test_x,km)
y_hat = y_hat %>% factor()
y_hat
ty = as.numeric(test_y) %>% factor() 
mean(y_hat == ty)
cm = confusionMatrix(y_hat, ty) 


train_glm <- train(train_x,train_y, method = "glm")
y_hat_glm <- predict(train_glm, test_x, type = "raw")
confusionMatrix(y_hat_glm, test_y)$overall[["Accuracy"]]

train_lda <- train(train_x,train_y, method = "lda")
y_hat_lda <- predict(train_lda, test_x, type = "raw")
confusionMatrix(y_hat_lda, test_y)$overall[["Accuracy"]]

train_qda <- train(train_x,train_y, method = "qda")
y_hat_qda <- predict(train_qda, test_x, type = "raw")
confusionMatrix(y_hat_qda, test_y)$overall[["Accuracy"]]

library(caret)
library(gam)
set.seed(5, sample.kind = "Rounding")    

train_loess <- train(train_x,train_y, method = "gamLoess")
y_hat_loess <- predict(train_loess, test_x, type = "raw")
confusionMatrix(y_hat_loess, test_y)$overall[["Accuracy"]]


set.seed(7, sample.kind = "Rounding")    
train_knn <- train(train_x,train_y, method = "knn", tuneGrid = data.frame(k = seq(3, 21, 2)))
train_knn$bestTune
y_hat_knn <- predict(train_knn, test_x, type = "raw")
confusionMatrix(y_hat_knn, test_y)$overall[["Accuracy"]]

library(randomForest)
set.seed(9, sample.kind="Rounding")
train_rpart <- train(train_x,train_y, method = "rf", tuneGrid = data.frame(mtry = 3), ntree=100)
y_hat_rp <- predict(train_rpart, test_x, type = "raw")
confusionMatrix(y_hat_rp, test_y)$overall[["Accuracy"]]


train_rf <- randomForest(train_x,train_y, tuneGrid = data.frame(mtry = 3), ntree=100)
varImpPlot(train_rf)
varImp(train_rf)


y_hat_km
class(y_hat_glm)
g = ifelse(y_hat_glm == "B",1,2)
l = ifelse(y_hat_lda == "B",1,2)
q = ifelse(y_hat_qda == "B",1,2)
lo = ifelse(y_hat_loess == "B",1,2)
kn = ifelse(y_hat_knn == "B",1,2)
rp = ifelse(y_hat_rp == "B",1,2)
g
p <- (y_hat_km + g + l + q + lo + kn + rp )/7
p
y_pred<-as.factor(ifelse(p>1.5,'M','B'))
confusionMatrix(y_pred, test_y)
