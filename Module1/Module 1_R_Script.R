getwd()
setwd("C:\\Users\\heath\\Documents\\CS 555\\Teaching\\Week1\\Module1")
getwd()


#1st Example - Qualitative Data summaries
#No row names in file is default. Header=TRUE is default.
data<-read.csv("ceo.csv", header=TRUE)
data

nrow(data)
ncol(data)
head(data)
tail(data)

table(data$edlevel)

relfreq<-table(data$edlevel)/nrow(data)*100
relfreq

sum(relfreq)

prop.table(table(data$edlevel))

barplot(table(data$edlevel))

barplot(relfreq)

barplot(relfreq, main="Bar Graph of Educ Level of CEOs", 
xlab = "Education Level" , ylab= "Relative Frequency",
col=c("navy","white","steelblue","paleturquoise2"))

barplot(relfreq, main="Bar Graph of Educ Level of CEOs", 
xlab = "Education Level" , ylab= "Relative Frequency",
col=gray(1-relfreq/100))

help(gray)

pie(table(data$edlevel), col=c("rosybrown1","white","steelblue","paleturquoise2"),
    main="Pie Chart of Educ Level of CEOs")

#install.packages("ggplot2")
library(ggplot2)

ggplot(data, aes(x=edlevel))+
geom_bar(width = .75, fill = c("lightsalmon","navy","lightseagreen","mediumorchid1"), color = "green")+
xlab("Education Level")+ylab("Count")+ggtitle("Bar Graph of Educ Level of CEOs")+ theme(plot.title = element_text(hjust = 0.5))


#2nd Example - Quantitative Data summaries

data <- read.csv("ages.csv")
data

mean(data$age)
median(data$age)
min(data$age)
max(data$age)
var(data$age)
sd(data$age)
sqrt(var(data$age))
quantile(data$age)
quantile(data$age,.15)
quantile(data$age,.10)

summary(data$age)
sd(data$age)



hist(data$age)
x<-hist(data$age, xlab="Age", main="Ages of CEOs", ylim=c(0,20))
x

hist(data$age,breaks=c(20,30,40,50,60,70,80,90))
hist(data$age,breaks=seq(20, 90, by=10))

help(hist)

hist(data$age,3)
hist(data$age,6)
hist(data$age,20)
hist(data$age,50)

par(mfrow=c(1, 1)) 
ggplot(data, aes(x=age))+geom_histogram(binwidth=10, fill = "white", color = "black") +
  scale_x_continuous(breaks = seq(15, 85, 10))

boxplot(data$age)


#Areas under Std Normal Curve


pnorm(1.53) 

qnorm(.9369916)


#Example - Birth Weights and the Normal Distribution

#a
pnorm(4100, mean=3500, sd = 500)
pnorm(1.2)

#or

z<-(4100-3500)/500
z
pnorm(z)

#b
1-pnorm(3200, mean=3500, sd = 500)

##or

pnorm(3200, mean=3500, sd = 500, lower.tail=FALSE)


#c
pnorm(3750, mean=3500, sd = 500) - pnorm(3250, mean=3500, sd = 500)


#Example - IQ scores
sd_xbar=10/sqrt(40)
mean_xbar=100

pnorm(95, mean=mean_xbar, sd = sd_xbar) - pnorm(90, mean=mean_xbar, sd = sd_xbar)

