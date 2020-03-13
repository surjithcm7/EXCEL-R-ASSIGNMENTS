comp_data <- read.csv(file.choose())
attach(comp_data)
summary(comp_data)
#there are no empty rows or columns
boxplot(comp_data)
#almost all the data are normal
#there are some outliers in the price data 
dim(comp_data)
str(comp_data)
comp_data$Sales <- cut(comp_data$Sales, breaks=c(-Inf,7.5, Inf),labels=c("low","high"))
comp_data$Sales
library(caret)
dt <- createDataPartition(comp_data$Sales, p = 0.80, list = F)
comp_train <- comp_data[dt,]
comp_test <- comp_data[-dt,]

dim(comp_train)
dim(comp_test)
library(randomForest)
comp.rf <- randomForest(Sales~., data = comp_train, importance= T)
plot(comp.rf)
p6 <- predict(comp.rf, newdata = comp_test, type = 'class')
confusionMatrix(p6,comp_test$Sales)
accur.rf <- confusionMatrix(p6,comp_test$Sales)$overall[1]
accur.rf
#accuracy is 87.34%
importance(comp.rf)
varImpPlot(comp.rf)











