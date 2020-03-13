#DT Based on company data
library(readr)
sales <- read.csv(file.choose())
View(sales)

#converting sales into a categorical variable

sales1 <- ifelse(sales$Sales <= 10, "bad", "good")
CompanyData <- data.frame(sales, sales1)
CompanyData <- CompanyData[,2:12]
View(CompanyData)
colnames(CompanyData)
#eda 
attach(CompanyData)
str(CompanyData)
summary(CompanyData)
hist(CompPrice)
hist(Income)
hist(Advertising)
hist(Population)
hist(Price)
hist(Age)
hist(Education)

plot(CompPrice)
plot(Income)
plot(Advertising)
plot(Population)
plot(Price)
plot(Age)
plot(Education)

#all data are normal
boxplot(CompPrice)
boxplot(Income)
boxplot(Advertising)
boxplot(Population)
boxplot(Price)
boxplot(Age)
boxplot(Education)


#splitting the data

CompanyData_train <- CompanyData[1:320,]
View(CompanyData_train)
CompanyData_test <- CompanyData[321:400,]
View(CompanyData_test)
# Building the model with 'ctree' function from 'party' package

library(party)
tree <- ctree(sales1 ~., data = CompanyData_train)
plot(tree)
pred <- predict(tree, newdata = CompanyData_test)
pred
mean(pred == CompanyData_test$sales1)
library(gmodels)
CrossTable(CompanyData_test$sales1, pred)
table <- table(CompanyData_test$sales1, pred)
table

# Accuracy = 85% which shows it as a good model.
#if the shelf location is good and price is <=118,there is 80% chance of high sales
#in a medium or bad shelf price <= have 50% of high sales.




