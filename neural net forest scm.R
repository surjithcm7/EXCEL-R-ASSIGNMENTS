#nn on forest fire
forestfire <- read.csv(file.choose())
View(forestfire)
str(forestfire)
attach(forestfire)

summary(forestfire)
dim(forestfire)

#first 2 columns day and month are irrelevant  and last column are redundant and removing those
Forestfire <- forestfire[,-c(1,2,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)]
dim(Forestfire)

# custom normalization function
#normalize to make all same standard
normalize <- function(x) { 
  return((x - min(x)) / (max(x) - min(x)))
}

Forest_norm<-as.data.frame(lapply(Forestfire,normalize))
summary(Forest_norm)

#Dividing the dataset into training and test data

ForestFire_train <- Forest_norm[1:362,]
View(ForestFire_train)
ForestFire_test <- Forest_norm[363:517,]
View(ForestFire_test)

## Training a model on the data ----
# train the neuralnet model
library(neuralnet)
library(nnet)
# simple ANN with only a single hidden neuron
Forest_model <- neuralnet(area~.,data = ForestFire_train)
str(Forest_model)
plot(Forest_model)

## Evaluating model performance 
model_results <- compute(Forest_model,ForestFire_test)
predicted_strength <- model_results$net.result
predicted_strength
cor(predicted_strength,ForestFire_test$area)  #finding correlation between actual and predicated value
plot(Forest_model)

## Improving model performance ----
# a more complex neural network topology with 5 hidden neurons
Forest_model1 <- neuralnet(area~.,data = ForestFire_train,hidden = 5)
str(Forest_model1)
plot(Forest_model1)
## Evaluating model performance 
model_results1 <- compute(Forest_model1,ForestFire_test)
predicted_strength1 <- model_results1$net.result
predicted_strength1
cor(predicted_strength1,ForestFire_test$area)  #finding correlation between actual and predicated value
plot(Forest_model1)
#correlation is incresed to 0.68% and error is 0.56 which is very less

# next model
Forest_model2 <- neuralnet(area~.,data = ForestFire_train,hidden = 15)
str(Forest_model2)
plot(Forest_model2)
## Evaluating model performance 
model_results2 <- compute(Forest_model2,ForestFire_test)
predicted_strength2 <- model_results1$net.result
predicted_strength2
cor(predicted_strength2,ForestFire_test$area)  #finding correlation between actual and predicated value
plot(Forest_model2)
# correlation is still .68% .












