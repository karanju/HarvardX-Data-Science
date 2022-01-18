library(tidyverse)
library(dslabs)
set.seed(16, sample.kind = "Rounding")
act_scores = rnorm(10000,20.9,5.7)
hist(act_scores)
mean(act_scores)
sd(act_scores)
a = (act_scores == 36)
sum(a)
mean(a)
a = (act_scores > 30)
sum(a)
mean(a)
a = (act_scores < 10)
sum(a)
mean(a)
mean(act_scores < 10)

x = seq(1,36)
f_x = function(x){
  dnorm(x,20.9,5.7)
}

plot(x,f_x(x))

m = mean(act_scores)
s = sd(act_scores)
a = 2*s + m
mean(act_scores>a)
a = qnorm(0.975,m,s)
a

x = seq(1,36)
func = function(x){
       pnorm(x,20.9,5,7)
} 

cdf = func(x)
a = cdf >= 0.95
a
pnorm(30,m,s)
pnorm(31,m,s)
qnorm(0.95,20.9,5.7)
qnorm(0.95,20.9,5.7)

p <- seq(0.01, 0.99, 0.01)
func = function(x){
        qnorm(x,m,s)} 

sample_quantiles = sapply(p,func)
sample_quantiles
a = sample_quantiles >= 26
a

p <- seq(0.01, 0.99, 0.01)
func = function(x){
  qnorm(x,20.9,5.7)} 
theoretical_quantiles = sapply(p,func)

qqplot(theoretical_quantiles,sample_quantiles)

sample_quantiles <- seq(0.01, 0.99, 0.01)
quantile(act_scores, sample_quantiles)

theoretical_quantiles <- qnorm(sample_quantiles, 20.9, 5.7)
qqplot(y=sample_quantiles, x=theoretical_quantiles)