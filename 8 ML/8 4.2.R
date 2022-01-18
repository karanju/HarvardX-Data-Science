set.seed(1996, sample.kind="Rounding") #if you are using R 3.6 or later
n <- 1000
p <- 10000
x <- matrix(rnorm(n*p), n, p)
colnames(x) <- paste("x", 1:ncol(x), sep = "_")
y <- rbinom(n, 1, 0.5) %>% factor()

x
x_subset <- x[ ,sample(p, 100)]
y
fit <- train(x_subset, y, method = "glm")
fit$results

install.packages("BiocManager")
BiocManager::install("genefilter")
library(genefilter)
tt <- colttests(x, y)
tt


pvals <- tt$p.value
pvals
class(pvals)
ind <- which(pvals <= 0.01)
ind
x_subset = x[,ind]
x_subset

fit <- train(x_subset, y, method = "glm")
fit$results

fit <- train(x_subset, y, method = "knn", tuneGrid = data.frame(k = seq(101, 301, 25)))
ggplot(fit)


library(dslabs)
data("tissue_gene_expression")

set.seed(1)
y <- tissue_gene_expression$y
x <- tissue_gene_expression$x
fit <- train(x, y, method = "knn", tuneGrid = data.frame(k = seq(1,7,2)))
ggplot(fit)




set.seed(1995)
n <- 10^6
income <- 10^(rnorm(n, log10(45000), log10(3)))
qplot(log10(income), bins = 30, color = I("black"))
m <- median(income)
m
N <- 100
X <- sample(income, N)
median(X)

library(gridExtra)
B <- 10^4
M <- replicate(B, {
  X <- sample(income, N)
  median(X)
})
M
p1 <- qplot(M, bins = 30, color = I("black"))
p2 <- qplot(sample = scale(M), xlab = "theoretical", ylab = "sample") + 
  geom_abline()
grid.arrange(p1, p2, ncol = 2)



library(gridExtra)
B <- 10^4
M <- replicate(B, {
  X <- sample(income, N)
  median(X)
})
p1 <- qplot(M, bins = 30, color = I("black"))
p2 <- qplot(sample = scale(M)) + geom_abline()
grid.arrange(p1, p2, ncol = 2)

mean(M)
sd(M)


B <- 10^4
M_star <- replicate(B, {
  X_star <- sample(X, N, replace = TRUE)
  median(X_star)
})

tibble(monte_carlo = sort(M), bootstrap = sort(M_star)) %>%
  qplot(monte_carlo, bootstrap, data = .) + 
  geom_abline()



quantile(M, c(0.05, 0.95))
quantile(M_star, c(0.05, 0.95))

median(X) + 1.96 * sd(X) / sqrt(N) * c(-1, 1)

mean(M) + 1.96 * sd(M) * c(-1,1)

mean(M_star) + 1.96 * sd(M_star) * c(-1, 1)



library(dslabs)
library(caret)
data(mnist_27)
set.seed(1995, sample.kind="Rounding") # if R 3.6 or later, set.seed(1995, sample.kind="Rounding")
indexes <- createResample(mnist_27$train$y, 10)
a = indexes[1]
class(a)
b=as.numeric(as.character(unlist(a)))
class(b)
sum(b==7)
b=as.numeric(as.character(unlist(indexes)))
sum(b==3)

y <- rnorm(100, 0, 1)
qnorm(0.75)
y
quantile(y, 0.75)

set.seed(1, sample.kind="Rounding")
B <- 10^4
q <- replicate(B, {
  y <- rnorm(100, 0, 1)
  quantile(y, 0.75)
})
mean(q)
sd(q)


set.seed(1)
y <- rnorm(100, 0, 1)
y
set.seed(1)
indexes <- createResample(y, 10)
indexes

q = sapply(indexes,function(ind)
    {
      x = y[ind]
      quantile(x, 0.75)
})
mean(q)
sd(q)


set.seed(1)
y <- rnorm(100, 0, 1)
y
set.seed(1)
indexes <- createResample(y, 10000)
indexes

q = sapply(indexes,function(ind)
{
  x = y[ind]
  quantile(x, 0.75)
})
mean(q)
sd(q)
