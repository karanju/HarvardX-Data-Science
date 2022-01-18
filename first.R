data(murders)
library(murders)
library(dslabs)
class(murders)
str(murders)
head(murders)
names(murders)
pop = murders$population
pop
length(pop)
levels(murders$region)
region <- murders$region
value <- murders$total
region <- reorder(region, value, FUN = sum)
levels(region)
mat <- matrix(1:12, 4, 3)
mat
as.data.frame(mat)
murders[2:3, ]
a = murders$abb
class(a)
b = murders[["abb"]]
class(b)
identical(a,b)
table(murders$region)
seq(1, 10, 2)
a = c("Beijing", "Lagos", "Paris")
b = c(35, 88, 42)
names(b) = a
b[1:2]
d = seq(12,75)
d
e = seq(6,55,4/7)
e
length(e)
class(e)
a = seq(1,10)
a
class(a)
ind = order(murders$total)
ind
murders$state[ind]
pop = sort(murders$population)
pop[1]
min(pop)
ind = order(murders$population)
ind
a = murders$state[ind]
a
which.min(ind)
ranks = rank(murders$population)
ranks
my_df = data.frame(name = murders$state, pop = ranks)
ind = order(murders$population)
my_df = data.frame(name = murders$state[ind], pop = ranks[ind])
my_df
data("na_example")  
str(na_example)
mean(na_example)
len = is.na(na_example)!=TRUE
b = len == TRUE
length(b)
s = sum(is.na(na_example))
s
a = is.na(na_example)
a
b = na_example[!a]
b
mean(b)
