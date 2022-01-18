library(tidyverse)
library(dslabs)
data(death_prob)
head(death_prob)

death_prob$sex
death_prob$age
p = (death_prob$age == 50 & death_prob$sex == "Female")  
death_prob$prob[p]

e = 0.003193*(-150000) + (1-0.003193)*1150
e
se = abs(1150+150000)*sqrt(0.003193*(1-0.003193))
se
1000*e
sqrt(1000)*se

pnorm(0,667378.1,269657.9)

p = (death_prob$age == 50 & death_prob$sex == "Male")  
death_prob$prob[p]

x = (700 + 0.005013*150000)/(1-0.005013)
x
e = 1000*((1-0.005013)*x-0.005013*150000)
se = abs(x+150000)*sqrt(0.005013*(1-0.005013)*1000)
se
pnorm(0,e,se)

e = 0.015*(-150000) + (1-0.015)*1150
e = 1000*e
e
se = (1150+150000)*sqrt(1000*0.015*(1-0.015))
se
pnorm(0,e,se)

pnorm(-1000000,e,se)

p <- seq(.01, .03, .001)
func = function(p){
       e = (p*(-150000) + (1-p)*1150)*1000
       se = (1150+150000)*sqrt(1000*p*(1-p))
       pnorm(0,e,se)
}

S = sapply(p,func)
S

p <- seq(.01, .03, .0025)
func = function(p){
  e = (p*(-150000) + (1-p)*1150)*1000
  se = (1150+150000)*sqrt(1000*p*(1-p))
  pnorm(-1000000,e,se)
}

S = sapply(p,func)
S


set.seed(25, sample.kind = "Rounding")

p_loss = 0.015
S = sample(c(-150000,1150),1000,replace=TRUE,prob=c(p_loss,1-p_loss))
sum(S)
sum(S)/(10^6)

set.seed(27, sample.kind = "Rounding")
B = 10000
T = replicate(B,{
  S = sample(c(-150000,1150),1000,replace=TRUE,prob=c(p_loss,1-p_loss))
  sum(S)
 })


mean(T<(-1000000))

n = 1000
l = -150000
p = 0.015
z = qnorm(0.05)
x = -l*( n*p - z*sqrt(n*p*(1-p)))/ ( n*(1-p) + z*sqrt(n*p*(1-p)))

t = l*p + x*(1-p) 
t*1000

set.seed(28, sample.kind = "Rounding")
B = 10000
T = replicate(B,{
  S = sample(c(-150000,x),1000,replace=TRUE,prob=c(p,1-p))
  sum(S)
})

mean(T<0)


set.seed(29, sample.kind = "Rounding")

p <- .015
profit <- replicate(B, {
  new_p <- p + sample(seq(-0.01, 0.01, length = 100), 1)
  draws <- sample( c(x, l), n, 
                   prob=c(1-new_p, new_p), replace = TRUE)
  sum(draws)
})
a = mean(profit)
a
b = mean(profit<0)
b
c = mean(profit<(-1000000))
c
