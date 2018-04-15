insurance <- read.csv("insurance.csv", stringsAsFactors = F)
str(insurance)
summary(insurance$charges)
hist(insurance$charges)
table(insurance$region)
cor(insurance[c("age", "bmi", "children", "charges")])
pairs(insurance[c("age", "bmi", "children", "charges")])

install.packages("psych")
library(psych)
pairs.panels(insurance[c("age", "bmi", "children", "charges")])

ins_model <- lm(charges ~ age + children + bmi + sex + smoker + region, data=insurance)
#ins_model <- lm(charges ~ ., data=insurance)
ins_model
summary(ins_model)
#Multiple R-squared : 0.7509
#the more stars it has, the more significant the variable is

insurance$age2 <- insurance$age^2
ins_model2 <- lm(charges ~ age + age2, data=insurance)
summary(ins_model2)
#Multiple R-squared : 0.09059 -> bad result

ins_model3 <- lm(charges ~ age + age2 + children + bmi + sex + smoker + region, data=insurance)
summary(ins_model3)
#Multiple R-squared : 0.7537
#the smaller number of variables with higher Multiple R-squared value is the best!

insurance$bmi30 <- ifelse(insurance$bmi >= 30, 1, 0)
ins_model4 <- lm(charges ~ age + children + bmi30 + sex + smoker + region, data=insurance)
summary(ins_model4)

ins_model5 <- lm(charges ~ age + age2 + children + bmi + bmi30*smoker, data=insurance)
#ins_model5 <- lm(charges ~ age + age2 + children + bmi + sex + bmi30*smoker + region, data=insurance)
summary(ins_model5)
#Multiple R-squared : 0.8645