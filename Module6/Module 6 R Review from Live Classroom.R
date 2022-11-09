

#one sample tests for proportions


#Example - Vaccines:  

prop.test(70, 100, p = 0.80,
          alternative = "two.sided",
          conf.level = 0.95, correct = FALSE)


#two sample tests for proportions

#Example in  notes - Need for Social Services. Manual calculations gave: 
#z = 2.32, p = 0.0204,  CI=( 0.033, 0.347)

prop.test(c(49,38), c(61,62), alternative = "two.sided",
          conf.level = 0.95, correct = FALSE)


p1 <- 49/61
p2 <- 38/62

p1-p2  #risk difference
p1/p2  #risk ratio
(p1/(1-p1))/(p2/(1-p2))  #odds ratio

#logistic regression
#Example - Risk of Coronary Event

#read in data
data<-read.csv("C:/Users/Yidow/Desktop/cevent.csv")

#simple logistic regression model 
m<-glm(data$event ~ data$chol, family = binomial)
summary(m)

exp(.02119*10)

#ORs per 1 unit increase
# same as calculation by hand (OR):  exp(0.02119)
# same as calculation by hand (OR 95% CI lower):  exp((m$coefficients[2]- qnorm(0.975)*summary(m)$coefficients[2,2]))
# same as calculation by hand (OR 95% CI upper):  exp((m$coefficients[2]+ qnorm(0.975)*summary(m)$coefficients[2,2]))
exp(cbind(OR = coef(m), confint.default(m)))


#OR per 10 unit increase
# same as calculation by hand (OR):  exp(0.02119*10)
exp(m$coefficients[2]*10)
exp((m$coefficients[2]- qnorm(0.975)*summary(m)$coefficients[2,2])*10)
exp((m$coefficients[2]+ qnorm(0.975)*summary(m)$coefficients[2,2])*10)


#predicted risk for each patient
risk<-predict(m,type=c("response"))
risk

#predicted risk for patient with chol of 190:   exp(-3.12716+0.02119*190)/ (1+exp(-3.12716+0.02119*190)) 
risk[41]
exp(m$coefficients[1]+m$coefficients[2]*190)/ (1+exp(m$coefficients[1]+m$coefficients[2]*190))


#multiple logistic regression model
data$male<-ifelse(data$sex == "M", 1, 0)
m2<-glm(data$event ~ data$chol+data$male + data$age, family = binomial)
summary(m2)

#overall test
#install.packages("aod")
library(aod)
wald.test(b = coef(m2), Sigma = vcov(m2), Terms = 2:4)

#ORs per 1 unit increase
exp(cbind(OR = coef(m2), confint.default(m2)))

#ROC curve
#install.packages("pROC")
library(pROC)

#using model with chol and sex and age
data$prob<-predict(m2,type=c("response"))  
g <- roc(data$event ~ data$prob)
plot(1-g$specificities, g$sensitivities, type = "l",
xlab = "1 - Specificity", ylab = "Sensitivity", main = "ROC Curve")
abline(a=0,b=1)
grid()


#using model with just chol
data$prob<-predict(m,type=c("response"))   
data$prob
g <- roc(data$event ~ data$prob) 
plot(1-g$specificities, g$sensitivities, type = "l",
xlab = "1 - Specificity", ylab = "Sensitivity", main = "ROC Curve")
abline(a=0,b=1)
grid()