beads = rep( c("red","blue"), times = c(2,3))
beads
sample(beads,1)

B = 10000
events = replicate(B, sample(beads,1))
events
tab = table(events)
tab
prop.table(tab)

sample(beads,B,replace=TRUE)
prop.table(table(events))

library(gtools)
library(tidyverse)

a = permutations(5,3)
a


runners <- c("Jamaica", "Jamaica", "Jamaica", "USA", "Ecuador", "Netherlands", "France", "South Africa")
B = 10000
sample(runners,3)
res = replicate(B,{
  win = sample(runners,3)
  mean(win == "Jamaica") == 1
})
mean(res)

a = combinations(6,3)
a
nrow(a)
b = 270*20/15
b
com = 7*nrow(a)*3
com
B = 10000
N = seq(1,12)
func = function(n){
  a = combinations(6,2)
  com = n*nrow(a)*3
  com > 365
}

res = sapply(N,func)
res

B = 10000
N = seq(2,12)
func = function(n){
  a = combinations(n,2)
  com = 6*nrow(a)*3
  com > 365
}
res = sapply(N,func)
res
a = combinations(7,2)
com = 6*nrow(a)*3
com


library(tidyverse)
head(esoph)
esoph$agegp
levels(esoph$agegp)
nrow(esoph)
levels(esoph$ncases)
esoph$ncases
all_cases = sum(esoph$ncases)
all_controls = sum(esoph$ncontrols)
all_controls
levels(esoph$alcgp)
a = ( (esoph$alcgp == "120+" & esoph$ncases>0) == TRUE )
esoph$ncases
(esoph$ncases[a])
b = ( (esoph$alcgp == "120+" & esoph$ncases>0) == FALSE )
sum(esoph$ncases[b])
esoph$alcgp
prab  = mean(a)
a = esoph$alcgp == "120+"
ca = sum(esoph$ncases[a])
co = sum(esoph$ncontrols[a])
a = esoph$alcgp == "0-39g/day"
ca = sum(esoph$ncases[a])
co = sum(esoph$ncontrols[a])
p = ca/(ca+co)
pra = mean(a)
p = prab/pra

levels(esoph$tobgp)
a = esoph$tobgp != "0-9g/day"
sum(esoph$ncases[a]) + sum(esoph$ncontrols[a])
ca = sum(esoph$ncases[a])
co  = sum(esoph$ncontrol[a])
p = ( co/975)
p

a = esoph$tobgp == "30+"
b = sum(esoph$ncases[a])

a = ( esoph$tobgp == "30+" & esoph$alcgp == "120+")
b = sum(esoph$ncases[a])
b

a = ( esoph$tobgp == "30+" | esoph$alcgp == "120+")
b = sum(esoph$ncases[a])
b

a = esoph$alcgp == "120+"
co = sum(esoph$ncontrols[a])
ca = sum(esoph$ncases[a])
ca
co

a = esoph$tobgp == "30+"
b = sum(esoph$ncontrols[a])


a = ( esoph$tobgp == "30+" & esoph$alcgp == "120+")
b = sum(esoph$ncontrols[a])
b

a = ( esoph$tobgp == "30+" | esoph$alcgp == "120+")
b = sum(esoph$ncontrols[a])
b