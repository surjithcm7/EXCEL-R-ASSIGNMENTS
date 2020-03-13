library(readr)
fraud <- read.csv(file.choose())
colnames(fraud)
str(fraud)

# Categorising Data as Risky or Good

Risk <- ifelse(fraud$Taxable.Income <= 30000, "Risky", "good")
FraudCheck <- data.frame(fraud[-3], Risk)
View(FraudCheck)

#EDA 
str(FraudCheck)
hist(fraud$Taxable.Income)
boxplot(fraud$Taxable.Income)
hist(fraud$City.Population)
hist(fraud$Work.Experience)
boxplot(fraud$Work.Experience)
#here we can see that the data is normal

# Splitting Data into Train and Test Components

FraudCheck_train <- FraudCheck[1:420,]
FraudCheck_test <- FraudCheck[421:600,]
View(FraudCheck_test)
View(FraudCheck_train)

# Building model with C5.0
library(C50)
tree = C5.0(FraudCheck_train[,-6], FraudCheck_train$Risk)
plot(tree)

pred <- predict(tree, newdata = FraudCheck_test)
mean(pred == FraudCheck_test$Risk) # Accuracy = 83%

# Cross Table

library(gmodels)
CrossTable(FraudCheck_test$Risk, pred)
library(caret)
confusionMatrix(FraudCheck_test$Risk, pred)


