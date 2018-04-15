install.packages("arules")
library(arules)
groceries <- read.transactions("groceries.csv", sep=",")
summary(groceries)

inspect(groceries[1:5])
itemFrequency(groceries[, 1:3])
itemFrequencyPlot(groceries, support=0.1)
itemFrequencyPlot(groceries, topN = 20)
image(groceries[1:5])
image(sample(groceries,100))

groceryrules <- apriori(groceries, parameter = list(support=0.006, confidence=0.25, minlen=2))

groceryrules
summary(groceryrules)

inspect(groceryrules[1:3])
inspect(sort(groceryrules, by="lift")[1:5])
berryrules <- subset(groceryrules, items %in% "berries")
inspect(berryrules)

berry_yogurt_rules <- subset(groceryrules, items %in% c("berries","yogurt"))
inspect(berry_yogurt_rules)

berry_yogurt_all_rules <- subset(groceryrules, items %ain% c("berries", "yogurt"))
inspect(berry_yogurt_all_rules)
