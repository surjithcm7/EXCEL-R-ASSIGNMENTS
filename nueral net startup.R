

library(readr)
Startup <- read.csv(file.choose())
View(Startup)
str(Startup)
attach(Startup)

summary(Startup)
sum(is.na(Startup))
dim(Startup)


#We don't need state column so we will remove it.
Startup <- Startup[-c(4)]
str(Startup)


normalize<-function(x){                # target variable is numerical hence we have to normalize 
  return ( (x-min(x))/(max(x)-min(x)))
}
Startup_norm<-as.data.frame(lapply(Startup,FUN=normalize))
summary(Startup_norm$Profit)
summary(Startup$Profit)

#Dividing the data into training and testing data
Startup_train <- Startup[1:35,]
View(Startup_train)
Startup_test <- Startup[36:50,]
View(Startup_test)

dim(Startup_train)
dim(Startup_test)

# Using multilayered feed forward nueral network
# package nueralnet

library(neuralnet)
library(nnet)

# Building model
Startup_model <- neuralnet(Profit~.,data = Startup_train , hidden = 5, stepmax = 1e6)
str(Startup_model)
plot(Startup_model)


# model with least sum of squared error is the best model
# Evaluating model performance
# compute function to generate ouput for the model prepared
model_results <- compute(Startup_model,Startup_test[1:4])
predicted_strength <- model_results$net.result
predicted_strength
model_results$neurons
cor(predicted_strength,Startup_test$Profit)  #finding correlation between actual and predicated value
plot(predicted_strength,Startup_test$Profit)
model_5<-neuralnet(Profit~.,data= Startup_norm,hidden = c(5,4,3))#building a model using hidden layers and performing it  on entire dataset

plot(model_5)

# the error has decreased to 0.05 which is very less
model_5_res<-compute(model_5,Startup_test[1:4])
pred_strn_5<-model_5_res$net.result
cor(pred_strn_5,Startup_test$Profit)
plot(pred_strn_5,Startup_test$Profit)
# SSE has reduced and training steps had been increased as the number of nuerons 
# under hidden layer are increased
