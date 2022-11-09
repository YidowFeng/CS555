setwd("C:\\Users\\heath\\Documents\\CS 555\\Teaching\\Week2\\Module2")


#Functions for the t distribution to get 
# (a) the probability to the left of a given value (pt)
# (b) the value of t for a given area to the left (qt)
pt(2.776, df = 4)
qt(0.975, df = 4)
qt(0.025, df = 4)

#1 - One Sample Tests - z test
data <- read.csv("weightchange.csv")

#install.packages("asbio")
library(asbio)

data
mean(data$wtchg)
sd(data$wtchg)

#can either type in the values or use them directly from data (to avoid rounding issues)
one.sample.z(null.mu=0,xbar=-2.98,sigma=6,n=30,alternative="less")

one.sample.z(null.mu=0,xbar=mean(data$wtchg),sigma=6,n=nrow(data),alternative="less")

##critical value approach
qnorm(.05)

# install.packages("BSDA")
library(BSDA)

##Can use this code to get 95% CI. Must specify two-sided test.
z.test(data$wtchg, y = NULL, alternative = "two.sided", mu = 0, sigma.x =6,
       sigma.y = NULL, conf.level = 0.95)


#2 - One Sample Tests - t test 

length <- c(18.1,23.4,23.9,24.1,22.5,19,25.4,23.1,16.5,26.7)
mean(length)
sd(length)

t.test(length, mu=20,  alternative="greater")

##To look at confidence interval
t.test(length, mu=20,  alternative="two.sided", conf.level=0.90)

##Critical value approach
qt(.90, 9)



#3 - Two Sample Tests - t test 
data <- read.csv("polyester.csv")
data

#can separate data into two separate datasets based on group
two<-data$brk[data$weeks==2]
sixteen<-data$brk[data$weeks==16]

mean(two)
mean(sixteen) 
mean(two) - mean(sixteen)

t.test(data$brk[data$weeks==2],data$brk[data$weeks==16], alternative="greater")

##For confidence interval
t.test(two,sixteen, alternative="two.sided", conf.level = .90)



#4 - Two Sample Numerical Summaries
data <- read.csv("scores.csv")
data

means<-aggregate(data$score, by=list(data$group), FUN = mean) 
means

tapply(data$score,data$group,summary)



#4 - Two Sample Graphical Summaries

boxplot(data$score~data$group)

barplot(means$x, names.arg= c("Yes","No"), xlab = "Volunteered in Past Year", main = "Mean Attachment Scores by Volunteer History"
, ylab = "Mean Attachment Score", col = "seagreen1", ylim=c(0,120))
abline(h=0)

#install.packages("gplots")
library(gplots)

attach(data)
means<-tapply(score,group,mean)
lower<-tapply(score,group,function(v) t.test(v)$conf.int[1])
upper<-tapply(score,group,function(v) t.test(v)$conf.int[2])

barplot2(means, plot.ci = TRUE, ci.l = lower, ci.u = upper,
names.arg= c("Yes","No"), xlab = "Volunteered in Past Year", main = "Mean Attachment Scores by Volunteer History"
, ylab = "Mean Attachment Score", col = "seagreen1",ylim=c(0,120) )
abline(h=0)


