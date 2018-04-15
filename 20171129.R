credit <- read.csv("credit.csv")
head(credit)

install.packages("caret")
library(caret)
set.seed(300)

install.packages("C50")
library(C50)
m <- train(default ~ ., data=credit, method="C5.0")
str(m)
m

p <- predict(m, credit)
table(p)
p
credit$default
table(p, credit$default)

head(predict(m, credit, type="raw"))
head(predict(m, credit, type="prob"))

ctrl <- trainControl(method="cv", number=10, selectionFunction = "oneSE")
grid <- expand.grid(.model="tree", .trials=c(1,5,10,15,20,25,30,35), .winnow="FALSE")
grid

set.seed(300)
m <- train(default ~ ., data=credit, method = "C5.0", metric="Kappa", trControl=ctrl, tuneGrid=grid)
m


install.packages("ipred")
library(ipred)
set.seed(300)
mybag <- bagging(default ~ ., data = credit, nbagg = 25)
credit_pred <- predict(mybag, credit)
table(credit_pred, credit$default)

library(caret)
set.seed(300)
ctrl <- trainControl(method = "cv", number = 10)
train(default ~ ., data = credit, method = "treebag", trControl = ctrl)

str(svmBag)
svmBag$fit

bagctrl <- bagControl(fit = svmBag$fit, predict = svmBag$pred, aggregate = svmBag$aggregate)

set.seed(300)
svmbag <- train(default ~ ., data = credit, "bag", trControl = ctrl, bagControl = bagctrl)
svmbag

install.packages("adabag")
library(adabag)
set.seed(300)
bst <- boosting.cv(default ~ ., data = credit, mfinal = 50)
bst$confusion
bst$error

install.packages("randomForest")
library(randomForest)
set.seed(300)
rf <- randomForest(default ~ ., data = credit)
rf

library(caret)
ctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 10)
grid_rf <- expand.grid(.mtry = c(2,4,8,16))

set.seed(300)
m_rf <- train(default ~ ., data = credit, method = "rf", metric = "Kappa", trControl = ctrl, tuneGrid = grid_rf)
m_rf

grid_c50 <- expand.grid(.model = "tree", .trials = c(10,20,30,40), .winnow = "FALSE")
set.seed(300)
m_c50 <- train(default ~ ., data = credit, method = "C5.0", metric = "Kappa", trControl = ctrl, tuneGrid = grid_c50)
m_c50






