library(tidyverse)
library(dslabs)
take_poll(25)   

sample(seq(0,1,length=100),100,replace=TRUE)
seq(0,1,length=100)

X_hat <- 0.48
se <- sqrt(X_hat*(1-X_hat)/100)
pnorm(0.01/se) - pnorm(-0.01/se)
pnorm(1)-pnorm(-1)

take_poll(1000)
take_poll(5)   

p <- 0.45    # unknown p to estimate
N <- 1000

# simulate one poll of size N and determine x_hat
x <- sample(c(0,1), size = N, replace = TRUE, prob = c(1-p, p))
x_hat <- mean(x)

# simulate B polls of size N and determine average x_hat
B <- 10000    # number of replicates
N <- 1000    # sample size per replicate
x_hat <- replicate(B, {
  x <- sample(c(0,1), size = N, replace = TRUE, prob = c(1-p, p))
  mean(x)
})

library(gridExtra)


p1 <- data.frame(x_hat = x_hat) %>%
  ggplot(aes(x_hat)) +
  geom_histogram(binwidth = 0.005, color = "black")
p2 <- data.frame(x_hat = x_hat) %>%
  ggplot(aes(sample = x_hat)) +
  stat_qq(dparams = list(mean = mean(x_hat), sd = sd(x_hat))) +
  geom_abline() +
  ylab("X_hat") +
  xlab("Theoretical normal")
grid.arrange(p1, p2, nrow=1)

library(tidyverse)
N <- 100000
p <- seq(0.35, 0.65, length = 100)
SE <- sapply(p, function(x) 2*sqrt(x*(1-x)/N))
data.frame(p = p, SE = SE) %>%
  ggplot(aes(p, SE)) +
  geom_line()


N <- seq(100, 5000, len = 100)
p <- 0.5
se <- sqrt(p*(1-p)/N)
plot(N,se)


p <- 0.45
N <- 1000

x <- sample(c(0, 1), size = N, replace = TRUE, prob = c(1-p, p))
x_hat <- mean(x)
se_hat <- sqrt(x_hat * (1 - x_hat) / N)
c(x_hat - 1.96 * se_hat, x_hat + 1.96 * se_hat)


z <- qnorm(0.99)
pnorm(z) - pnorm(-z)

B <- 10000
inside <- replicate(B, {
  X <- sample(c(0,1), size = N, replace = TRUE, prob = c(1-p, p))
  X_hat <- mean(X)
  SE_hat <- sqrt(X_hat*(1-X_hat)/N)
  between(p, X_hat - 2*SE_hat, X_hat + 2*SE_hat)    # TRUE if p in confidence interval
})
mean(inside)

N <- 100
z <- sqrt(N)*0.02/0.5
z
pnorm(z)
pnorm(z)-pnorm(-z)
1- ( pnorm(z) - pnorm(-z) )


data(polls_us_election_2016)
head(polls_us_election_2016)
polls = filter(polls_us_election_2016, enddate >= "2016-10-31" & state == "U.S.")
rows = nrow(polls)
rows
head(polls)
N = polls$samplesize[1]
N
X_hat = polls$rawpoll_clinton[1] / 100
se_hat = sqrt(X_hat*(1-X_hat)/N)
X_hat*(1-X_hat)/N

(X_hat + 2*se_hat)
pnorm(X_hat + 2*se_hat)
pnorm(3)
ci = c(X_hat - qnorm(0.975)*se_hat, X_hat + qnorm(0.975)*se_hat)
ci
z = qnorm(0.975)
z

X_hat = polls$rawpoll_clinton / 100
se_hat = sqrt(X_hat*(1-X_hat)/N)
lower = X_hat - qnorm(0.975)*se_hat 
upper = X_hat + qnorm(0.975)*se_hat 

pollster_results = mutate(polls,X_hat,se_hat,lower,upper)
pollster_results = select (pollster_results,pollster,enddate,X_hat,se_hat,lower,upper )

hit = (0.482>=pollster_results$lower & pollster_results$upper>=0.482)
pollster_results = mutate(pollster_results,hit)
avg_hit <- mean(pollster_results$hit)


X_hat = (polls$d_hat+1)/2
se_hat = 2*sqrt(X_hat*(1-X_hat)/polls$samplesize)
lower = polls$d_hat - qnorm(0.975)*se_hat
upper = polls$d_hat + qnorm(0.975)*se_hat

pollster_results = mutate (polls,X_hat,se_hat,lower,upper)
pollster_results = select (pollster_results,pollster,enddate,d_hat,lower,upper)