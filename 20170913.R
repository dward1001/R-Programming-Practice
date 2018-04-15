getwd()

name <- c("tom")
name

subject_name <- c("Jon-Lark Kim","Jane Lee","Patrick Park")
subject_name

temperature <- c(36.1, 36.5, 40.1)
temperature

flu_status <- c(FALSE, FALSE, TRUE)
flu_status

temperature[1]
temperature[2]

temperature[1:3]

temperature[-2]

temperature[c(TRUE, TRUE, FALSE)]
temperature[-3]
temperature[-1:-3]

temperature[c(TRUE, TRUE, FALSE)] == temperature[-3]

blood <- factor(c("O","AB","A"))
blood

gender <- factor(c("Male", "Female", "Male"))
gender

blood <- factor(c("O", "AB", "A"), levels = c("A", "B", "AB", "O"))
blood

subject1 <- list(fullname = subject_name[1], temperature = temperature[1], 
                 flu_status = flu_status[1], gender = gender[1], blood = blood[1])
subject1
subject1[2]

subject1$temperature
subject1["temperature"]

subject1[2] == subject1$temperature     #considering the internal value only -> equal

subject2 <- list(fullname = subject_name[2], temperature = temperature[2], 
                 flu_status = flu_status[2], gender = gender[2], blood = blood[2])
subject2

subject3 <- list(fullname = subject_name[3], temperature = temperature[3], 
                 flu_status = flu_status[3], gender = gender[3], blood = blood[3])
subject3

# DATA FRAME
pt_data <- data.frame(subject_name, temperature, flu_status,
                      gender, blood, stringsAsFactors = FALSE)
# stringsAsFactors : whether all the character input are defined as factors
# safe to set as FALSE / no error even if set as TRUE

pt_data
pt_data$subject_name

pt_data$subject_name == subject_name
#results for each information : TRUE TRUE TRUE

pt_data[c("temperature", "flu_status")]

pt_data[1,2]    #impossible if vector / now possible since array

pt_data[c(1,3), c(2,4)]   # 1st and 3rd rows & 2nd and 4th column
pt_data[,1]   # 1st column
pt_data[1,]   # 1st row
pt_data[-2,c(-1,-3,-5)]   # deleting 2nd row & 1st, 3rd, 5th columns

pt_data[c(1,3), c(2,4)] == pt_data[-2,c(-1,-3,-5)]
str(pt_data)

# MATRIX
m <- matrix(c('a', 'b', 'c', 'd'), nrow=2) # default : column first
m

mdat <- matrix(c(1,2,3, 11,12,13), nrow=2, ncol=3, byrow=TRUE, dimnames=list(c("row1","row2"),
                                                                             c("C.1","C.2","C.3")))
# byrow : row first / dimnames : labeling the names
mdat

# READING & WRITING CSV FILES
pt_data <- read.csv("Pt_data.csv", stringsAsFactors = FALSE)

pt_data
write.csv(pt_data, file="pt_data_write.csv")

usedcars <- read.csv("usedcars.csv", stringsAsFactors = FALSE)
usedcars
str(usedcars)

summary(usedcars$year)
summary(usedcars[c("price", "mileage")])
summary(usedcars)

# mean median range diff
mean(c(20000, 30000, 40000))  #average
median(c(20000, 35000, 40000))  #value at the middle
mean(c(20000, 35000, 40000))

mean(usedcars$price)
summary(usedcars$price)
range(usedcars$price)

diff(range(usedcars$price))
IQR(usedcars$price)     # 75% - 25%

quantile(usedcars$price)
quantile(usedcars$mileage)

quantile(usedcars$price, probs = c(0.01, 0.99))   #set the percentage
quantile(usedcars$price, seq(from=0, to=1, by=0.10))


#boxplots
boxplot(usedcars$price, main="Boxplot of Used Car Prices", ylab="Price ($)")
boxplot(usedcars$mileage, main="Boxplot of Used Car Mileage", ylab="Odometer (mi.)")

hist(usedcars$price, main="histogram of Used Car Prices", xlab="Price($)")
hist(usedcars$mileage, main="histogram of Used Car Mileage", xlab="Odometer($)")

var(usedcars$price)     #variance
sd(usedcars$price)      #standard deviance
var(usedcars$mileage)
sd(usedcars$mileage)

sd(usedcars$mileage)^2 == var(usedcars$mileage)

str(usedcars)     #structure

table(usedcars$year)
table(usedcars$model)
table(usedcars$color)
table(usedcars$transmission)

model_table <- table(usedcars$model)
prop.table(model_table)     # probability

color_table <- table(usedcars$color)
color_pct <- prop.table(color_table)*100
round(color_pct, digits=1)    # show only one number under the point

# ScatterPlot
plot(x=usedcars$mileage, y=usedcars$price, main="Scatterplot of Price vs. Mileage",
     xlab="Used Car Odometer(mi)", ylab="Used Car Price($)")
plot(x=usedcars$mileage, y=usedcars$year, main="Scatterplot of Year vs. Mileage",
     xlab="Used Car Odometer(mi)", ylab="Used Car Year")

# CrossTable
install.packages("gmodels")
library(gmodels)
usedcars$conservative <- usedcars$color %in% c("Black", "Gray", "Silver", "White")
table(usedcars$conservative)

CrossTable(x=usedcars$model, y=usedcars$conservative)
CrossTable(x=usedcars$model, y=usedcars$conservative, chisq=TRUE)

