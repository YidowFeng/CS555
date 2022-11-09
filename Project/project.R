#read data
data <- read.csv(file='C:/Users/Yidow/Desktop/CS555/Project/breast_cancer.csv', 
                 fileEncoding="UTF-8-BOM")
col_name <- names(data)

#data cleaning
for (i in 1:ncol(data)-1) {
  print(paste(col_name[i],cor(data$Classification, data[,i])))
}
data <- subset(data, select = c("Glucose","Insulin","HOMA","Resistin","Classification"))
head(data)

#graph
par(mar=c(1,1,1,1))
plot(data$Insulin,data$HOMA,
     main="Scatterplot of HOMA and Insulin",
     xlab = "HOMA", ylab="Insulin", pch=19)
abline(lm(data$HOMA~data$Insulin))
cor(data$Insulin,data$HOMA)

par(mfrow=c(2,2))
Glucose_level <-ifelse(data$Glucose <= 99 & data$Glucose >= 70, 1, 0)
hist(Glucose_level)
Insulin_level <-ifelse(data$Insulin < 17, 1, 0)
hist(Insulin_level)
HOMA_level <-ifelse(data$HOMA < 1, 1, 0)
hist(HOMA_level)
Resistin_level <-ifelse(data$Resistin <= 22 & data$Resistin >= 7, 1, 0)
hist(Resistin_level)

#logistic regression
Glucose_level <-ifelse(data$Glucose <= 99 & data$Glucose >= 70, 1, 0)
table(Glucose_level, data$Classification)
pat_p <- 34/64
heal_p <- 43/52
prop.test(c(34,43), c(64,52), alternative = "two.sided",
          conf.level = 0.95, correct = FALSE)

Healthy_controls <- ifelse(data$Classification == 1, 1, 0)
m <- glm(Glucose_level ~ Healthy_controls, family = binomial)
summary(m)

#least sqaure
m3 <- lm(data$Classification~(data$Glucose+data$Insulin+data$HOMA+data$Resistin))
summary(m3)
qf(0.95,df1=3,df2=102-3-1)