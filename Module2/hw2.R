
data <- read.csv(file = 'C:/Users/Yidow/Desktop/child.csv', fileEncoding="UTF-8-BOM")
data

cal_part <- data$Calorie.Intake.for.participants
cal_no_part <- data$Calorie.intake.for.non.participants[!is.na(data$Calorie.intake.for.non.participants)]
summary(cal_part)
sd(cal_part)
#population sd
sd(cal_part)*sqrt((25-1)/25)
summary(cal_no_part)
sd(cal_no_part)

h_part <- hist(cal_part)
text(h_part$mids,h_part$counts,labels=h_part$counts, adj=c(0.5, -0.5))
h_part

h_no_part <- hist(cal_no_part)
text(h_no_part$mids,h_no_part$counts,labels=h_no_part$counts, adj=c(0.5, -0.5))
h_no_part

one.sample.z(null.mu=425, xbar=mean(cal_part),sigma=119.0587, n=25, alternative = "two.sided", conf=.95)

410.1-1.645*(121.5138/sqrt(25)) 
410.1+1.645*(121.5138/sqrt(25))

length(cal_part[cal_part>370.122 &cal_part<450.078])/25

t.test(cal_part,cal_no_part)
