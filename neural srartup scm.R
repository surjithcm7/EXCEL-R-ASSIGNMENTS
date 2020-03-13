#NN on 50 startups
library(readr)
Startup <- read.csv(file.choose())
View(Startup)
str(Startup)
summary(Startup)
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

## Training a model on the data ----
# train the neuralnet model
library(neuralnet)
library(nnet)
# simple ANN with 5 hidden neuron

Startup_model <- neuralnet(Profit~.,data = Startup_train,hidden = 5,stepmax = 1e6)
str(Startup_model)
plot(Startup_model)

## Evaluating model performance 
model_results <- compute(Startup_model,Startup_test)
predicted_strength <- model_results$net.result
predicted_strength
cor(predicted_strength,Startup_test$Profit)  #finding correlation between actual and predicated value
plot(Startup_model)


#improving model
Startup_model1 <- neuralnet(Profit~.,data = Startup_train,hidden = 10,stepmax = 1e6)
str(Startup_model1)
plot(Startup_model1)

## Evaluating model performance 
model_results1 <- compute(Startup_model1,Startup_test)
predicted_strength1 <- model_results1$net.result
predicted_strength1
cor(predicted_strength1,Startup_test$Profit)  #finding correlation between actual and predicated value
plot(Startup_model1)

#improving model
startup_model2<-neuralnet(Profit~.,data= Startup_norm,hidden = c(5,4,3))#building a model using hidden layers and performing it  on entire dataset

plot(startup_model2)

# the error has decreased to 0.05 which is very less
model_results2<-compute(startup_model2,Startup_test[1:4])
pred_strn_2<-model_results2$net.result
cor(pred_strn_2,Startup_test$Profit)
plot(pred_strn_2,Startup_test$Profit)
# SSE has reduced and training steps had been increased as the number of nuerons 
# under hidden layer are increased






