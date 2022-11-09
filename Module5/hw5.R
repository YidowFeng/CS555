data <- read.csv(file = 'C:/Users/Yidow/Desktop/IQ.csv', fileEncoding="UTF-8-BOM")

nrow(data[data$group == "Chemistry student",])
nrow(data[data$group == "Math student",])
nrow(data[data$group == "Physics student",])

boxplot(iq~group, data = data, main = "IQ by Student Group",
        xlab = "Group",
        ylab = "IQ")

boxplot(age~group, data = data, main = "Age by Student Group",
        xlab = "Group",
        ylab = "Age")

#2
m0<-aov(iq~group, data = data)  
m0
summary(m0)
#Pairwise Comparisons
pairwise.t.test(data$iq, data$group, p.adj = "none")
pairwise.t.test(data$iq, data$group, p.adj = "bonferroni")

#Difference in reference group
TukeyHSD(m0)

#3
data$Physics<-ifelse(data$group == "Physics student", 1, 0)
data$Math<-ifelse(data$group == "Math student", 1, 0)
data$Chemistry<-ifelse(data$group == "Chemistry student", 1, 0)

m1<-lm(iq~Physics+Math, data = data) #Set chemistry students as the reference group
summary(m1)
m2<-lm(iq~Chemistry+Math, data = data) #Set Physics students as the reference group
summary(m2)
m3<-lm(iq~Physics+Chemistry, data = data) #Set Math students as the reference group
summary(m3)

#4
library(car)
m4<-Anova(lm(iq~group+age, data=data), type=3)
m4
summary(m4)
library(lsmeans)
options(contrasts=c("contr.treatment","contr.poly"))
lsmeans(lm(iq~group+age, data=data), pairwise ~ group, adjust =
            "none")
