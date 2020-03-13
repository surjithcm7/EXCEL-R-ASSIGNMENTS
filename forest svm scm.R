#forestfire
forest <- read.csv(file.choose())
View(forest)
str(forest)
attach(forest)

summary(forest)
dim(forest)

#first 2 columns day and month are irrelevant  and last column are redundant and removing those
Forestfire <- forest[,-c(1,2,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)]
dim(Forestfire)

# custom normalization function
#normalize to make all same standard
normalize <- function(x) { 
  return((x - min(x)) / (max(x) - min(x)))
}

Forest_norm<-as.data.frame(lapply(Forestfire,normalize))
summary(Forest_norm)
ff <- cbind(Forest_norm,forest$size_category)
#making a new data
write.csv(ff,file ="forestnew" )
getwd()
#Dividing the dataset into training and test data
forest1 <- read.csv(file.choose())
View(forest1)
dim(forest1)
forest1 <- forest1[,-c(1)]

library(caret)
Index <- createDataPartition(forest1$forest.size_category, p=0.7, list=FALSE) 
#spliting the data into train and test using the ratio 7:3
forest_train <- forest1[ Index,] 
View(forest_train)
forest_test <- forest1[-Index,] 
View(forest_test)
dim(forest_test)

#model creation #vanilladot kernel
library(kernlab)
model1<-ksvm(forest.size_category~temp+rain+wind+RH+area+FFMC+DMC+DC+ISI, 
             data= forest_train,kernel = "vanilladot")


#basic information about kernal
model1

#evaluating model performance
#prediction on testing dataset

model1_pred <- predict(model1,forest_test)
head(model1_pred)
table(model1_pred,forest_test$forest.size_category)

agreement <- model1_pred == forest_test$forest.size_category
table(agreement)
prop.table(table(agreement))
mean(model1_pred==forest_test$forest.size_category)
# accuracy 0.87 %

#improving model performance
#accuracy can be changed by using different kernel.
model2<-ksvm(forest.size_category~temp+rain+wind+RH+area+FFMC+DMC+DC+ISI, 
             data= forest_train,kernel = "rbfdot")

model2_pred <- predict(model2,forest_test)
head(model2_pred)
table(model2_pred,forest_test$forest.size_category)

agreement1 <- model2_pred == forest_test$forest.size_category
table(agreement1)
prop.table(table(agreement1))
mean(model2_pred==forest_test$forest.size_category)
#0.79 accuracy reduced.

#polydot kernel
modelply<-ksvm(forest.size_category~temp+rain+wind+RH+area+FFMC+DMC+DC+ISI, 
             data= forest_train,kernel = "polydot")


modelply_pred <- predict(modelply,forest_test)
head(modelply_pred)
table(modelply_pred,forest_test$forest.size_category)

agreement2<- modelply_pred == forest_test$forest.size_category
table(agreement2)
prop.table(table(agreement2))
mean(modelply_pred==forest_test$forest.size_category)
#0.87 model has improved.


