url <- paste0("https://en.wikipedia.org/w/index.php?title=",
              "Gun_violence_in_the_United_States_by_state",
              "&direction=prev&oldid=810166167")
library(tidyverse)
library(rvest)

h <- read_html(url)
h

html_text(h)
tab <- h %>% html_nodes("table")
tab
tab[[1]]
tab <- tab[[1]] %>% html_table
class(tab)
head(tab)

tab <- tab %>% setNames(c("state", "population", "total", "murders", "gun_murders", "gun_ownership", "total_rate", "murder_rate", "gun_murder_rate"))
head(tab)


library(rvest)
url <- "https://web.archive.org/web/20181024132313/http://www.stevetheump.com/Payrolls.htm"
h <- read_html(url)
nodes <- html_nodes(h, "table")
nodes
html_text(nodes[[8]])
html_table(nodes[[8]])
html_table(nodes[[1]])
html_table(nodes[[2]])
html_table(nodes[[3]])
html_table(nodes[[4]])
html_table(nodes[[18]])
html_table(nodes[[19]])
html_table(nodes[[20]])

tab1 = html_table(nodes[[10]])
tab2 = html_table(nodes[[19]])
tab1 = tab1 %>% select(Team=X2,Payroll=X3,Average=X4)
tab2 = tab2 %>% select(Team=X1,Payroll=X2,Average=X3)
head(tab1)
tab1 = tab1[-1,]
tab2 = tab2[-1,]
head(tab2)
tab1 %>% full_join(tab2,by="Team")

library(rvest)
library(tidyverse)
url <- "https://en.wikipedia.org/w/index.php?title=Opinion_polling_for_the_United_Kingdom_European_Union_membership_referendum&oldid=896735054"

h <- read_html(url)
nodes <- html_nodes(h, "table")
nodes
colnames(html_table(nodes[[1]],fill=TRUE))
colnames(html_table(nodes[[2]],fill=TRUE))
colnames(html_table(nodes[[3]],fill=TRUE))
colnames(html_table(nodes[[4]],fill=TRUE))
colnames(html_table(nodes[[5]],fill=TRUE))
colnames(html_table(nodes[[6]],fill=TRUE))
colnames(html_table(nodes[[7]],fill=TRUE))
