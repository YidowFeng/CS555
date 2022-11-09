data <- read.csv(file = 'C:/Users/Yidow/Desktop/fish.csv', fileEncoding="UTF-8-BOM")
data

plot(data$Number.of.meals.with.fish,data$Total.Mercury.in.mg.g, 
     main="Scatterplot of Mercury versus Number of meals with fish",
     xlab = "Number of meals with fish", ylab="Total Mercury in mg/ga", pch=19)
abline(lm(data$Total.Mercury.in.mg.g~data$Number.of.meals.with.fish))

cor(data$Number.of.meals.with.fish,data$Total.Mercury.in.mg.g)

lm(data$Total.Mercury.in.mg.g~data$Number.of.meals.with.fish)

meals <- data$Number.of.meals.with.fish
mercury <- data$Total.Mercury.in.mg.g

anova(lm(mercury~meals))

summary(lm(mercury~meals))