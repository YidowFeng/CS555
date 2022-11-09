#setwd("C:\\Users\\heath\\Documents\\CS 555\\Teaching\\Week3\\Module3")

#Read in data
examdata <- read.csv(file = 'C:/Users/Yidow/Desktop/CS555/Module3/Exam Scores.csv')
examdata

#Basic Scatterplot
plot(examdata$Hours,examdata$Exam)

par(mfrow=c(1, 1)) 
#Scatterplot with labs, and controlling axes
plot(examdata$Hours,examdata$Exam, 
main="Scatterplot of Exam Score versus Hours of Study Time",
xlab = "Hours of Study Time", ylab="Exam Score", 
xlim=c(0, 10), ylim=c(45, 100), pch = 8, col="seagreen3", cex=1, cex.lab = 1, cex.main = 1)


#Calculate Sample Correlation
cor(examdata$Hours,examdata$Exam)
cor(examdata$Exam,examdata$Hours)

cor.test(examdata$Hours,examdata$Exam)
cor.test(examdata$Exam,examdata$Hours)

#Simple Linear Regression
lm(examdata$Exam~examdata$Hours)
m<-lm(examdata$Exam~examdata$Hours)
m

#Adding regression line to scatterplot
plot(examdata$Hours,examdata$Exam, 
main="Scatterplot of Exam Score versus Hours of Study Time",
xlab = "Hours of Study Time", ylab="Exam Score", 
xlim=c(0, 10), ylim=c(45, 100), pch = 1, 
 cex=1, cex.lab = 1.5, cex.main = 1.5)
abline(m,lty=3,col="red")

#Request important summary information from R about the model
anova(m)
summary(m)
confint(m, level = 0.95)
