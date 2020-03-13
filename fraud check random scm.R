fraud <- read.csv(file.choose())
View(fraud)
attach(fraud)

summary(fraud)

boxplot(fraud)
#almost all the data are normal
#there are no outliers in the data 
fraud$Taxable.Income <- cut(fraud$Taxable.Income, breaks=c(-10002,30000, Inf),labels=c("Risky","Good"))
fraud$Taxable.Income
dim(fraud)
str(fraud)
library(caret)
dt <- createDataPartition(fraud$Urban, p = 0.80, list = F)
train <- fraud[dt,]
test <- fraud[-dt,]

dim(train)
dim(test)
library(randomForest)
fraud.rf <- randomForest(Taxable.Income~., data = train, importance= T)
plot(fraud.rf)
p6 <- predict(fraud.rf, newdata = test, type = 'class')
confusionMatrix(p6,test$Taxable.Income)
accur.rf <- confusionMatrix(p6,test$Taxable.Income)$overall[1]
accur.rf
#accuracy is 73.31%
importance(fraud.rf)
varImpPlot(fraud.rf)
