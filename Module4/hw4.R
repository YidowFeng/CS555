data <- read.csv(file = 'C:/Users/Yidow/Desktop/canadian.csv', fileEncoding="UTF-8-BOM")

plot(data$Education.Level..years.,data$Prestige.Score,
     main="Scatterplot of Prestige Score vesus Years of Education",
     xlab = "Years of Education", ylab="Prestige Score", pch=19)
abline(lm(data$Prestige.Score~data$Education.Level..years.))

cor(data$Education.Level..years.,data$Prestige.Score)

m <- lm(data$Prestige.Score~data$Education.Level..years.)
summary(m)
anova(m)
confint(m, level=0.95)

par(mfrow=c(2,2))
plot(m)

plot(data$Education.Level..years.,resid(m), axes=TRUE, frame.plot=TRUE, xlab = "education", 
     ylab="residuals")
hist(resid(m))

#3
m3 <- lm(data$Prestige.Score~(data$Education.Level..years.+data$Income....+data$Percent.of.Workforce.that.are.Women))
summary(m3)
qf(0.95,df1=3,df2=102-3-1)

#4
confint(m3, level=0.95)

#5
par(mfrow=c(2,2))
plot(fitted(m3),resid(m3), axes=TRUE, frame.plot=TRUE, xlab = "fitted values", ylab="residuals")
abline(h=0)
plot(data$Education.Level..years.,resid(m3), axes=TRUE, frame.plot=TRUE, xlab = "education", ylab="residuals")
abline(h=0)
plot(data$Income....,resid(m3), axes=TRUE, frame.plot=TRUE, xlab = "income", ylab="residuals")
abline(h=0)

#Checking Normality of residuals
hist(resid(m3))

plot(m3)
