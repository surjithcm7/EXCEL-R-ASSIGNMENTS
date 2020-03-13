#naive bayes for salary data
library(readr)
Salary_train <- read.csv(file.choose())
Salary_test  <- read.csv(file.choose())
View(Salary_train)
View(Salary_test)
str(Salary_train)

Salary_train$educationno <- as.factor(Salary_train$educationno)
class(Salary_train)

Salary_test$educationno <- as.factor(Salary_test$educationno)
class(Salary_test)

#eda
plot(Salary_train$workclass,Salary_train$Salary)
plot(Salary_train$education,Salary_train$Salary)
plot(Salary_train$educationno,Salary_train$Salary)
plot(Salary_train$maritalstatus,Salary_train$Salary)
plot(Salary_train$occupation,Salary_train$Salary)
plot(Salary_train$relationship,Salary_train$Salary)
plot(Salary_train$race,Salary_train$Salary)
plot(Salary_train$sex,Salary_train$Salary)

##model
library(e1071)
Model <- naiveBayes(Salary_train$Salary ~ ., data = Salary_train)
Model
Model_pred <- predict(Model,Salary_test)
mean(Model_pred==Salary_test$Salary)
#0.81
library(caret)
confusionMatrix(Model_pred,Salary_test$Salary)
table(Model_pred)
prop.table(table(Model_pred))




