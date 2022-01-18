set.seed(21, sample.kind = "Rounding")
B = 10000
S = replicate(B,{
  a = sample(c(1,-0.25),44,replace=TRUE,prob=c(1/5,4/5))
  sum(a)
})
mean(S>=8)

func = function(p){
  S=replicate(B,{
    a = sample(c(1,0),44,replace=TRUE,prob=c(p,1-p))
    sum(a)
  })
  mean(S>35)
}
 
p <- seq(0.25, 0.95, 0.05)
S = sapply(p,func)
S
T = ceiling(S>0.8)
T

p=5/38
b = 6*5/38-33/38
m=500*b
a = 7*sqrt(p*(1-p))
s=a*sqrt(500)
a/sqrt(500)
pnorm(0,m,s)
