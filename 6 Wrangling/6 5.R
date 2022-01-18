fn <- system.file("extdata", "RD-Mortality-Report_2015-18-180531.pdf", package="dslabs")
system("cmd.exe", input = paste("start", fn))


library("pdftools")
txt <- pdf_text(fn)
file.remove(fn)

view(txt)
class(txt)
length(txt)
a = txt[9]
x = str_split(a, "\n")
head(x)
class(x)
length(x)

s = x[[1]]
class(s)
length(s)
s = str_trim(s)
s[1]
header_index = str_which(s, "2015")
header_index = header_index[1]
header = s[header_index]
header

a = str_split(header,"\\s+", simplify = TRUE)
month = a[1]
header = a[2:5]
header

s
tail_index = str_which(s, "Total")
tail_index

n = str_count(s,"^[0-9]+$")
n
s = s[-c(1:header_index,6,9,tail_index:40)]
s

s = str_remove_all(s, "[^\\d\\s]")
s

s <- str_split_fixed(s, "\\s+", n = 6)[,1:5]
s
colnames(s) = c("day",header)
s
s = as.numeric(s)
class(s)
s = matrix(data=s,ncol=5,nrow=30)
class(s)
tab = s 
tab

colMeans(tab)

mean(tab[1:19,4])
mean(tab[20:30,4])

tab
mean(s[[1]])

tab <- tab %>% gather(year, deaths, -day) %>%
  mutate(deaths = as.numeric(deaths))
tab

tab = tab %>%
  as.data.frame  %>% 
  gather(year, deaths, -day, convert = TRUE)  
tab
tab %>%
  ggplot(aes(x = day, y = deaths, group = year, col = year)) + geom_vline(xintercept = 20)

               