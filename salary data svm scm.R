

#Loading the training and testing data

Salary_train <- read.csv(file.choose())
Salary_test  <- read.csv(file.choose())

# kvsm() function uses gaussian RBF kernel 

# Building model 

library(kernlab)
library(caret)
# kernel = vanilladot
model_vanilla<-ksvm(Salary ~.,data = Salary_train,kernel = "vanilladot")
pred_vanilla<-predict(model_vanilla,newdata=Salary_test)
mean(pred_vanilla==Salary_test$Salary) 

#confusion matrix
confusionMatrix(pred_vanilla,Salary_test$Salary)
#accuracy of the model using vanilladot kernal is 84.63%

# kernel = rfdot 
model_rfdot<-ksvm(Salary ~.,data = Salary_train,kernel = "rbfdot")
pred_rfdot<-predict(model_rfdot,newdata=Salary_test)
mean(pred_rfdot==Salary_test$Salary)
#accuracy of the model using rbfdot kernal is  85.42%
#confusion matrix
confusionMatrix(pred_rfdot,Salary_test$Salary)



# kernel = rfdot 
model_rfdot<-ksvm(Salary ~.,data = Salary_train,kernel = "rbfdot")
pred_rfdot<-predict(model_rfdot,newdata=Salary_test)
mean(pred_rfdot==Salary_test$Salary)
#accuracy of the model using rbfdot kernal is  85.42%
#confusion matrix
confusionMatrix(pred_rfdot,Salary_test$Salary)


# kernal = besseldot
model_besseldot<-ksvm(Salary ~.,data = Salary_train,kernel = "besseldot")
pred_bessel<-predict(model_besseldot,newdata=Salary_test)
mean(pred_bessel==Salary_test$Salary)
confusionMatrix(pred_bessel,Salary_test$Salary)
## accuracy of the model using besseldot kernal is 77.03%

# kernel = polydot

model_poly<-ksvm(Salary ~.,data = Salary_train,kernel = "polydot")
pred_poly<-predict(model_poly,newdata = Salary_test)
mean(pred_poly==Salary_test$Salary) 
confusionMatrix(pred_poly,Salary_test$Salary)
#accuracy of the model with polydot kernal is 84.62%


#here we can say that model with rbfdot kernal has highest accuracy