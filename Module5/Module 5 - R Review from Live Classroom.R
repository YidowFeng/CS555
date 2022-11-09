library(plyr)
setwd("C:\\Users\\heath\\Documents\\CS 555\\Teaching\\Week5\\Module 5")

#Read in data
data <- read.csv("SBP.csv")
data


#Graphical Summary
boxplot(SBP~Group, data = data, main = "SBP by Smoking Category",
        xlab = "Group",
        ylab = "SBP (mmHg)",
        ylim = c(100, 160))


#Check if grouping variable is a factor
is.factor(data$Group)

#Creating factor variable 
data$grp<-factor(data$Group, levels = c(0,1,2,3))
is.factor(data$grp)
data$grpnew<-revalue(data$grp, c("0"="Heavy", "1"="Never", "2"="Former", "3"="Light"))
is.factor(data$grpnew)

boxplot(SBP~grpnew, data = data, main = "SBP by Smoking Category",
        xlab = "Group",
        ylab = "SBP (mmHg)",
        ylim = c(100, 160))

data$grp2<-factor(data$Group, levels = c(1,2,3,0))
data
data$grpnew2<-revalue(data$grp2, c("0"="Heavy", "1"="Never", "2"="Former", "3"="Light"))
is.factor(data$grp2)
is.factor(data$grpnew2)

boxplot(SBP~grpnew2, data = data, main = "SBP by Smoking Category",
        xlab = "Group",
        ylab = "SBP (mmHg)",
        ylim = c(100, 160))

#One way ANOVA using aov function
m0<-aov(SBP~Group, data = data)  #this is the wrong way as it is considering group to be numeric since group is not a factor
summary(m0)

##Correct way.
m<-aov(SBP~grp, data = data)
summary(m)

m<-aov(SBP~grpnew, data = data)
summary(m)

m2<- aov(SBP~grpnew2, data = data)
summary(m2)

#Pairwise Comparisons
pairwise.t.test(data$SBP, data$grpnew, p.adj = "none")
pairwise.t.test(data$SBP, data$grpnew, p.adj = "bonferroni")

#Difference in reference group
TukeyHSD(m)
TukeyHSD(m2)

#Creating dummy variables
data$g0<-ifelse(data$Group == 0, 1, 0)
data$g1<-ifelse(data$Group == 1, 1, 0)
data$g2<-ifelse(data$Group == 2, 1, 0)
data$g3<-ifelse(data$Group == 3, 1, 0)

#One way ANOVA using lm function - this is another option of how you can do this, 
#but more work manually.

m2<-lm(SBP~g0+g1+g2, data = data) #group 3 (Light Smokers) as reference
summary(m2)
m3<-lm(SBP~g1+g2+g3, data = data) #group 0 (Heavy Smokers) as reference
summary(m3)
m4<-lm(SBP~g0+g2+g3, data = data) #group 1 (Never Smokers) as reference
summary(m4)

#use caution when interpreting coefficients from lm function with factor 
#need to ensure contrasts are set up appropriately
#also need to check which is the reference group

options(contrasts=c("contr.treatment","contr.poly"))

#0,1,2,3 first group (g0) is the reference
m5<-lm(SBP~grp, data=data)
m5
summary(m5)

#same as
m6<-lm(SBP~g1+g2+g3, data=data)
m6
summary(m6)

#1,2,3,0 first group (g1) is the reference
lm(SBP~grp2, data=data)

lm(SBP~g2+g3+g0, data=data)


##ANCOVA

m7 <- lm(SBP ~grpnew+Age, data=data)
summary(m7)

#use Anova function from car package instead of aov function for ANCOVA
#install.packages("car")
library(car)

Anova(lm(SBP~grp+Age, data=data), type=3)

#Can also use Anova for one-way ANOVA
Anova(lm(SBP~grpnew, data=data))

#Results are identical to before
summary(aov(SBP~grpnew, data = data))


#Generate Least Squares means and comparisons

#install.packages("lsmeans")
library(lsmeans)
options(contrasts=c("contr.treatment","contr.poly"))

# Covariate-adjusted/Least Squares means and comparisons
lsmeans(m7, pairwise ~ grpnew, adjust = "none")

lsmeans(m7, pairwise ~ grpnew, adjust = "tukey")

##Different ages across groups.
ddply(data, "Group", summarise,
               N    = length(Age),
               mean = mean(Age),
               sd   = sd(Age) )


#Read in data

##An educator is interested in the effect of birth month and gender 
#on average reading and math scores for students in the fourth grade.

data <- read.csv("scores.csv")
data

#order variables and assign labels
data$gender<-factor(data$Gender, levels = c("M","F"))
data$bday<-factor(data$Birthday, levels = c(0,1))
data$bday<-revalue(data$bday, c("0"="Sep - Mar", "1"="Apr - Aug"))
data$gender<-revalue(data$gender, c("M"="Males", "F"="Females"))

#numerical summaries
ddply(data, c("bday", "gender"), summarise,
               N    = length(Math),
               mean = mean(Math),
               sd   = sd(Math) )

ddply(data, c("bday", "gender"), summarise,
               N    = length(Reading),
               mean = mean(Reading),
               sd   = sd(Reading) )

##Reading scores higher in apr-aug for females, but not true for males.


#test interactions
Anova(lm(Reading~bday+gender+bday*gender, data=data),type = 3)
Anova(lm(Math~bday+gender+bday*gender, data=data),type = 3)

#generate interaction plots
interaction.plot(data$bday, data$gender, data$Math, col=c("red", "black"), xlab = "Birthday Month", ylab="Mean Math Score", trace.label = "Gender")
interaction.plot(data$bday, data$gender, data$Reading,  col=c("red", "black"),xlab = "Birthday Month", ylab="Mean Reading Score", trace.label = "Gender")

#ignoring interaction can lead to issues in interpretation - do not run 2 way anova if interaction is significant!
Anova(lm(Reading~bday+gender, data=data),type = 3)

Anova(lm(Math~bday+gender, data=data), type = 3)


#if interaction is significant, need to stratify (by one of the two factors)
datamales<-data[which(data$gender=='Males'),]
datafemales<-data[which(data$gender=='Females'),]

summary(aov(Math~bday, data = datafemales))
summary(aov(Reading~bday, data = datafemales))

summary(aov(Math~bday, data = datamales))
summary(aov(Reading~bday, data = datamales))

