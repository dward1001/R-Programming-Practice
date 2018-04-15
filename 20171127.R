sms_results <- read.csv("sms_results.csv")
head(sms_results)
head(subset(sms_results, actual_type!=predict_type))
table(sms_results$actual_type, sms_results$predict_type)

install.packages("gmodels")
library(gmodels)
CrossTable(sms_results$actual_type, sms_results$predict_type)

install.packages("caret")
library(caret)
install.packages('e1071', dependencies=TRUE)
confusionMatrix(sms_results$actual_type, sms_results$predict_type, positive="spam")

pr_a <- 0.865 + 0.111
pr_a
pr_e <- 0.868*0.886 + 0.132*0.114
pr_e
k <- (pr_a - pr_e)/(1-pr_e)
k

install.packages("vcd")
library(vcd)
Kappa(table(sms_results$actual_type, sms_results$predict_type)) # unweighted value : 0.8867

install.packages("irr")
library(irr)
kappa2(sms_results[1:2]) # Kappa : 0.887





















