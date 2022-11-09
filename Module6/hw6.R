data <- read.csv(file = 'C:/Users/Yidow/Desktop/temperature.csv', fileEncoding="UTF-8-BOM")
data

temp_level <-ifelse(data$temp >= 98.6, 1, 0)
male<- data[data$sex == 1,]
female<- data[data$sex == 2,]
temp_level_male <- ifelse(male$temp >= 98.6, 1, 0)
temp_level_female <- ifelse(female$temp >= 98.6, 1, 0)

table(temp_level)
table(temp_level_male)
table(temp_level_female)


#boxplot(temp~sex, data=data)
# par(mfrow=c(1,2))
# pie(table(temp_level_male),labels = c("<98.6",">=98.6"), 
#     main="Pie Chart of Male")
# pie(table(temp_level_female),labels = c("<98.6",">=98.6"), 
#     main="Pie Chart of Female")
# hist(male$temp)
# hist(female$temp)
summary(male$temp)
summary(female$temp)

#3
prop.test(c(14,35), c(65,65), alternative = "two.sided",
          conf.level = 0.95, correct = FALSE)

#4
sex_01 <- ifelse(data$sex == 1, 1, 0)
m<-glm(sex_01 ~ data$temp, family = binomial)
summary(m)
exp(-1.1512)
exp(cbind(OR = coef(m), confint.default(m)))
data$prob<-predict(m,type=c("response"))   
library(pROC)
g <- roc(data$sex ~ data$prob) 
g
prop.test(81, 130, alternative = "two.sided",
          conf.level = 0.95, correct = FALSE)
p1<-0.5384615
p2<-0.2153846 
(p1/(1-p1))/(p2/(1-p2))
#5
m1 <- glm(temp_level~sex_01 + data$Heart.rate, family = binomial)
summary(m1)
exp(m$coefficients[2]*10)
roc(temp_level~sex_01+data$Heart.rate)

#6
data$prob<-predict(m1,type=c("response"))  
g <- roc(data$sex ~ data$prob)
par(mfrow=c(1,1))
plot(1-g$specificities, g$sensitivities, type = "l",
     xlab = "1 - Specificity", ylab = "Sensitivity", main = "ROC Curve")
abline(a=0,b=1)
grid()
