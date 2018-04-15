launch <- read.csv("challenger.csv")
launch

#correlation between temperature & distress_ct

b <- cov(launch$temperature, launch$distress_ct)/var(launch$temperature)
b

a <- mean(launch$distress_ct) - b*mean(launch$temperature)
a

r <- cov(launch$temperature, launch$distress_ct)/(sd(launch$temperature)*sd(launch$distress_ct))
r
cor(launch$temperature, launch$distress_ct)

reg <- function(y, x){
  x <- as.matrix(x)
  x <- cbind(Intercept=1, x)
  solve(t(x)%*%x) %*% t(x) %*% y
}
# solve == inverse

str(launch)

reg(y=launch$distress_ct, x=launch[3])
reg(y=launch$distress_ct, x=launch[3:5])


insurance <- read.csv("insurance.csv")
str(insurance)

summary(insurance$charges)
hist(insurance$charges)
table(insurance$region)

cor(insurance[c("age", "bmi", "children", "charges")])
pairs(insurance[c("age", "bmi", "children", "charges")])

install.packages("psych")
library(psych)
pairs.panels(insurance[c("age", "bmi", "children", "charges")])






