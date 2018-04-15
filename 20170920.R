wbcd <- read.csv("wisc_bc_data.csv", stringsAsFactors = FALSE)
str(wbcd)   #B:Benign / M:Malignant

wbcd <- wbcd[-1]    #deleting first column : ID
str(wbcd)
table(wbcd$diagnosis)

wbcd$diagnosis <- factor(wbcd$diagnosis, levels = c("B","M"), labels = c("Benign", "Malignant"))
table(wbcd$diagnosis)

round(prop.table(table(wbcd$diagnosis))*100, digits=1)
summary(wbcd[c("radius_mean", "area_mean", "smoothness_mean")])


# normalization
normalize <- function(x){
  return ((x-min(x))/(max(x)-min(x)))
}

normalize(c(1,2,3,4,5))
normalize(c(10,20,30,40,50))


# lapply(): apply the function to the list, not just a vector
lapply(wbcd[2:31], normalize)
wbcd_n <- as.data.frame(lapply(wbcd[2:31], normalize))
str(wbcd)
str(wbcd_n)

summary(wbcd_n$area_mean)
summary(wbcd$area_mean)

# making train data and test data
wbcd_train <- wbcd_n[1:469,]  # 469 is for convenience. usually 70%
wbcd_test <- wbcd_n[470:569,] # after , include all columns

wbcd_train_labels <- wbcd[1:469,1]
wbcd_test_labels <- wbcd[470:569, 1]

wbcd_train_labels
wbcd_test_labels

# modeling train using knn() function
install.packages("class")
library(class)  # don't forget this part

sqrt(469)  # hence choose k=21

# kNN algorithm format 
# p <- knn(train, test, class, k)
wbcd_test_pred <- knn(train=wbcd_train, test=wbcd_test, cl=wbcd_train_labels,
                      k=21)
wbcd_test_pred

# compare this with the actual result. most parts are the same
wbcd_test_labels  

# evaluation of the performance
install.packages("gmodels")
library(gmodels)

CrossTable(x=wbcd_test_labels, y=wbcd_test_pred, prop.chisq = FALSE)
# 2% error: look at x=Malignant, y=Benign part

# z score standardization
wbcd_z <- as.data.frame(scale(wbcd[-1]))
str(wbcd)
wbcd_z 
summary(wbcd_z$area_mean)

wbcd_train <- wbcd_z[1:469, ]
wbcd_test <- wbcd_z[470:569, ]
#wbcd_test_labels <- wbcd[1:469, 1]
wbcd_test_pred <- knn(train= wbcd_train, test=wbcd_test,
                      cl=wbcd_train_labels, k=21)
CrossTable(x=wbcd_test_labels, y=wbcd_test_pred, prop.chisq=FALSE)
# 2% error: : look at x=Malignant, y=Benign part


# k=1
wbcd_test_pred <- knn(train= wbcd_train, test=wbcd_test,
                      cl=wbcd_train_labels, k=1)
CrossTable(x=wbcd_test_labels, y=wbcd_test_pred, prop.chisq=FALSE)
#7% error

# k=10
wbcd_test_pred <- knn(train= wbcd_train, test=wbcd_test,
                      cl=wbcd_train_labels, k=10)
CrossTable(x=wbcd_test_labels, y=wbcd_test_pred, prop.chisq=FALSE)
# 1% error