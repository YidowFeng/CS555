data <- read.csv(file = 'C:/Users/Yidow/Desktop/hospital.csv', header = FALSE, fileEncoding="UTF-8-BOM")
data

list <- c()
for (i in 1:ncol(data)){
  list <- c(list,data[,i])
}
list

h <- hist(list, breaks=seq(1,15))
text(h$mids,h$counts,labels=h$counts, adj=c(0.5, -0.5))
h

summary(list)

pnorm(10, mean = 5, sd = 3)

1-pnorm(6, mean = 5, sd = 3/sqrt(35))