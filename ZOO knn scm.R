zoo <- read.csv(file.choose())
class(zoo)
zoo <- zoo[,-1]
colnames(zoo)
View(zoo)
# table of types.

table(zoo$type)
zoo$type<- factor(zoo$type, levels=c("1","2","3","4","5","6","7"),labels=c("airborne","aquatic","predator","toothed","backbone","venomous","domestic"))
View(zoo$type)

# table or proportation of enteries in the datasets. What % of entry of each type is there
round(prop.table(table(zoo$type))*100,1)
summary(zoo$type)
summary(zoo[c("hair","feathers","fins","legs","tail")])


#Creating a function to normalize the data
norm <- function(x){ 
  return((x-min(x))/(max(x)-min(x)))
}

#Applying the normalization function to the zoo dataset
zoo_norm<-as.data.frame(lapply(zoo[1:16],norm))
View(zoo_norm)
summary(zoo_norm)

#Create training and testing data
zoo_train<- zoo_norm[1:70,]
zoo_test <- zoo_norm[71:101,]

#Get labels for training and test datasets

zoo_train_label <- zoo[1:70,17]
zoo_test_label <- zoo[71:101,17]
View(zoo_train_label)
View(zoo_test_label)


# Build a KNN model on taining dataset
library("class")
library("caret")
# Building the KNN model on training dataset and also need labels which we are including c1. Once we build the preduction model
# we have to test on test dataset
zoo_pred <- knn(train = zoo_train , test = zoo_test, cl = zoo_train_label, k=10)
class(zoo_train)   
class(zoo_test)
## Now evualuating the model performance
## 

library("gmodels")

# Create cross table of predicted and actual
CrossTable( x =  zoo_test_label, y = zoo_pred)  
mean(zoo_pred==zoo_test_label)
#accuracy of the model is 70%
#that is the model is 77.4% accurate

#now we go for other values of k
zoo_pred1 <- knn(train = zoo_train , test = zoo_test, cl = zoo_train_label, k=10)
CrossTable( x =  zoo_test_label, y = zoo_pred1)
mean(zoo_pred1==zoo_test_label)
#accuracy of the model decresedto 74.1% for k = 10
zoo_pred2 <- knn(train = zoo_train , test = zoo_test, cl = zoo_train_label, k=12)
CrossTable( x =  zoo_test_label, y = zoo_pred2)
mean(zoo_pred2==zoo_test_label)
#accuracy=74%
zoo_pred3 <- knn(train = zoo_train , test = zoo_test, cl = zoo_train_label, k=18)
CrossTable( x =  zoo_test_label, y = zoo_pred3)
mean(zoo_pred3==zoo_test_label)
#accuracy=67%
zoo_pred4 <- knn(train = zoo_train , test = zoo_test, cl = zoo_train_label, k=11)
CrossTable( x =  zoo_test_label, y = zoo_pred4)
mean(zoo_pred4==zoo_test_label)
#accuracy= 74%
zoo_pred5 <- knn(train = zoo_train , test = zoo_test, cl = zoo_train_label, k=9)
CrossTable( x =  zoo_test_label, y = zoo_pred5)
mean(zoo_pred5==zoo_test_label)
#accuracy= 77%
#so here we can conclude that the model with k value = 10 is the best model. 
#here we can see that no airbone animal is classified incorrectly
#no aquatic animal is classified incorretly
#there are 4 predators in the original data 
#but in classification model 3 of them are classified as toothed animals
#and 1 as airbone animals
#toothed is classified correctly
#animal which was under backbone catogery is classified as domestic animal
#2 venomous animals are there and both are classified correctly
# out of 5 domestic 2 are classified as venomous and one is classified as toothed and 1 is classified correctly













