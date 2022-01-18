getwd()

path = system.file("extdata",package="dslabs")
path
list.files(path)

filename = "murders.csv"
fullpath = file.path(path,filename)
fullpath

file.copy(fullpath, getwd())

file.exists(filename)

library(tidyverse)
dat <- read_csv(filename)

library(readr)
read_lines("murders.csv", n_max = 3)
dat <- read_csv(filename)
View(dat)

dat <- read_csv(fullpath)
dat                        

dat2 <- read.csv(filename)
class(dat2$abb)
class(dat2$region)

url <- "https://raw.githubusercontent.com/rafalab/dslabs/master/inst/extdata/murders.csv"
dat <- read_csv(url)
View(dat)
download.file(url, "kfile.csv")

tmp_filename <- tempfile()
download.file(url, tmp_filename)
dat <- read_csv(tmp_filename)
file.remove(tmp_filename)

url = "http://mlr.cs.umass.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wdbc.data"
dar = read_csv(url)
dar
view(dar)
