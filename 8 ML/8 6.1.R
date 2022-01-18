library(dslabs)
mnist <- read_mnist()

names(mnist)
dim(mnist$train$images)

class(mnist$train$labels)
table(mnist$train$labels)

# sample 10k rows from training set, 1k rows from test set
set.seed(123)
index <- sample(nrow(mnist$train$images), 10000)
x <- mnist$train$images[index,]
y <- factor(mnist$train$labels[index])

index <- sample(nrow(mnist$test$images), 1000)
#note that the line above is the corrected code - code in video at 0:52 is incorrect
x_test <- mnist$test$images[index,]
y_test <- factor(mnist$test$labels[index])


library(matrixStats)
sds <- colSds(x)
qplot(sds, bins = 256)

library(caret)
nzv <- nearZeroVar(x)
image(matrix(1:784 %in% nzv, 28, 28))

col_index <- setdiff(1:ncol(x), nzv)
length(col_index)

colnames(x) <- 1:ncol(mnist$train$images)
colnames(x_test) <- colnames(x)

control <- trainControl(method = "cv", number = 10, p = .9)
train_knn <- train(x[ ,col_index], y, 
                   method = "knn", 
                   tuneGrid = data.frame(k = c(3,5,7)),
                   trControl = control)


n <- 1000
b <- 2
index <- sample(nrow(x), n)
control <- trainControl(method = "cv", number = b, p = .9)
train_knn <- train(x[index ,col_index], y[index],
                   method = "knn",
                   tuneGrid = data.frame(k = c(3,5,7)),
                   trControl = control)

train_knn
ggplot(train_knn)

fit_knn <- knn3(x[ ,col_index], y,  k = 3)

y_hat_knn <- predict(fit_knn,
                     x_test[, col_index],
                     type="class")
cm <- confusionMatrix(y_hat_knn, factor(y_test))
cm$overall["Accuracy"]

cm$byClass[,1:2]


library(Rborist)
control <- trainControl(method="cv", number = 5, p = 0.8)
grid <- expand.grid(minNode = c(1,5) , predFixed = c(10, 15, 25, 35, 50))
train_rf <-  train(x[, col_index], y,
                   method = "Rborist",
                   nTree = 50,
                   trControl = control,
                   tuneGrid = grid,
                   nSamp = 5000)
ggplot(train_rf)
train_rf$bestTune

fit_rf <- Rborist(x[, col_index], y,
                  nTree = 1000,
                  minNode = train_rf$bestTune$minNode,
                  predFixed = train_rf$bestTune$predFixed)

y_hat_rf <- factor(levels(y)[predict(fit_rf, x_test[ ,col_index])$yPred])
cm <- confusionMatrix(y_hat_rf, y_test)
cm$overall["Accuracy"]

rafalib::mypar(3,4)
for(i in 1:12){
  image(matrix(x_test[i,], 28, 28)[, 28:1], 
        main = paste("Our prediction:", y_hat_rf[i]),
        xaxt="n", yaxt="n")
}


library(randomForest)
x <- mnist$train$images[index,]
y <- factor(mnist$train$labels[index])
rf <- randomForest(x, y,  ntree = 50)
imp <- importance(rf)
imp


image(matrix(imp, 28, 28))

p_max <- predict(fit_knn, x_test[,col_index])
p_max
p_max <- apply(p_max, 1, max)
p_max
ind  <- which(y_hat_knn != y_test)
ind <- ind[order(p_max[ind], decreasing = TRUE)]
rafalib::mypar(3,4)
for(i in ind[1:12]){
  image(matrix(x_test[i,], 28, 28)[, 28:1],
        main = paste0("Pr(",y_hat_knn[i],")=",round(p_max[i], 2),
                      " but is a ",y_test[i]),
        xaxt="n", yaxt="n")
}


p_rf <- predict(fit_rf, x_test[,col_index])$census
p_rf <- p_rf / rowSums(p_rf)
p_knn <- predict(fit_knn, x_test[,col_index])
p <- (p_rf + p_knn)/2
y_pred <- factor(apply(p, 1, which.max)-1)
confusionMatrix(y_pred, y_test)





library(caret)
library(dslabs)
set.seed(1, sample.kind = "Rounding") # use `set.seed(1, sample.kind = "Rounding")` in R 3.6 or later
data("mnist_27")

models <- c("glm", "lda", "naive_bayes", "svmLinear", "knn", "gamLoess", "multinom", "qda", "rf", "adaboost")

fits <- lapply(models, function(model){ 
  print(model)
  train(y ~ ., method = model, data = mnist_27$train)
}) 

fits
names(fits) <- models

mf <- sapply(fits, function(fit){
  
  y_hat <- predict(fit, mnist_27$test, type = "raw")
})

mf
dim(mf)
length(mnist_27$test$y)
length(models)

acc <- colMeans(mf == mnist_27$test$y)
acc
mean(acc)
sum(acc > mean(acc))


racc <- rowMeans(mf=="7")
racc
ac = ifelse(racc>0.5,7,2)
ac
acc = (ac == mnist_27$test$y)
mean(acc)

cvfits <- sapply(fits, function(fit){ 
          min(fit$results$Accuracy)
})

cvfits
mf[,cvfits>mean(cvfits)]

racc <- rowMeans(mf[,cvfits>mean(cvfits)]=="7")
racc
ac = ifelse(racc>0.5,7,2)
ac
acc = (ac == mnist_27$test$y)
mean(acc)


library(dslabs)
library(dplyr)
library(tidyverse)
data("tissue_gene_expression")
dim(tissue_gene_expression$x)

tissue_gene_expression

pc <- prcomp(tissue_gene_expression$x)
t = data.frame(pc_1 = pc$x[,1], pc_2 = pc$x[,2],tissue = tissue_gene_expression$y)

t %>% ggplot(aes(x=pc_1,y=pc_2,color=tissue)) + geom_point()

tissues = levels(tissue_gene_expression$y)
tissues
x = tissue_gene_expression$x
t = t %>% mutate(rm = rowMeans(x))
view(t)
t = t %>% mutate(r = cor(pc_1,rm) )

t %>% ggplot(aes(x=pc_1,y=rm,color=tissue)) + geom_point()

x <- with(tissue_gene_expression, sweep(x, 1, rowMeans(x)))
pc <- prcomp(x)
data.frame(pc_1 = pc$x[,1], pc_2 = pc$x[,2], 
           tissue = tissue_gene_expression$y) %>%
  ggplot(aes(pc_1, pc_2, color = tissue)) +
  geom_point()

t = data.frame(pc_1 = pc$x[,1], pc_2 = pc$x[,2], 
               tissue = tissue_gene_expression$y)

t = t %>% mutate(pc_7=pc$x[,7])
t %>% ggplot(aes(y=pc_7,col=tissue)) + geom_boxplot()

for(i in 1:10){
  boxplot(pc$x[,i] ~ tissue_gene_expression$y, main = paste("PC", i))
}

boxplot(pc$x[,7] ~ tissue_gene_expression$y, main = paste("PC", i))

plot(summary(pc)$importance[3,])
view(summary(pc))
pc
