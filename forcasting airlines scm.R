#forcasting airlines data

library(readxl)
library(forecast)
library(fpp)
library(smooth)

Airlines<-read_excel(file.choose()) 
View(Airlines)  # Seasonality 12 months
plot(Airlines$Passengers,type="o")

# Pre Processing

# So creating 12 dummy variables 

X<- data.frame(outer(rep(month.abb,length = 96), month.abb,"==") + 0 )# Creating dummies for 12 months
View(X)

colnames(X)<-month.abb # Assigning month names 
View(X)
AirlineData<-cbind(Airlines,X)
View(AirlineData)

#input t
AirlineData["t"]<- 1:96 # adding t variable
View(AirlineData)

#adding log and t^2 values.
AirlineData["log_Passengers"]<-log(AirlineData["Passengers"])
AirlineData["t_square"]<-AirlineData["t"]*AirlineData["t"]
attach(AirlineData)
View(AirlineData)

#data partition,creating test and traing data

train<-AirlineData[1:84,]
test<-AirlineData[85:96,]
View(train)
View(test)

############################linear model#####################################

linear_model<-lm(Passengers~t,data=train)
summary(linear_model)
#predicting a model on test data
linear_pred<-data.frame(predict(linear_model,interval='predict',newdata =test))  
View(linear_pred)
rmse_linear<-sqrt(mean((test$Passengers-linear_pred$fit)^2,na.rm = T)) #na.rm=T means removing any na values
rmse_linear
# 53.19

########################## Exponential #################################

expo_model<-lm(log_Passengers~t,data=train)
summary(expo_model)
expo_pred<-data.frame(predict(expo_model,interval='predict',newdata=test))
rmse_expo<-sqrt(mean((test$Passengers-exp(expo_pred$fit))^2,na.rm = T))
rmse_expo 
#46.02

######################### Quadratic ####################################

Quad_model<-lm(Passengers~t+t_square,data=train)
summary(Quad_model)
Quad_pred<-data.frame(predict(Quad_model,interval='predict',newdata=test))
rmse_Quad<-sqrt(mean((test$Passengers-Quad_pred$fit)^2,na.rm=T))
rmse_Quad 
# 48.05

######################### Additive Seasonality #########################

sea_add_model<-lm(Passengers~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)
summary(sea_add_model)
sea_add_pred<-data.frame(predict(sea_add_model,newdata=test,interval='predict'))
rmse_sea_add<-sqrt(mean((test$Passengers-sea_add_pred$fit)^2,na.rm = T))
rmse_sea_add 
# 132.81

######################## Additive Seasonality with Linear #################

Add_sea_Linear_model<-lm(Passengers~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)
summary(Add_sea_Linear_model)
Add_sea_Linear_pred<-data.frame(predict(Add_sea_Linear_model,interval='predict',newdata=test))
rmse_Add_sea_Linear<-sqrt(mean((test$Passengers-Add_sea_Linear_pred$fit)^2,na.rm=T))
rmse_Add_sea_Linear 
#35.34

######################## Additive Seasonality with Quadratic #################

Add_sea_Quad_model<-lm(Passengers~t+t_square+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)
summary(Add_sea_Quad_model)
Add_sea_Quad_pred<-data.frame(predict(Add_sea_Quad_model,interval='predict',newdata=test))
rmse_Add_sea_Quad<-sqrt(mean((test$Passengers-Add_sea_Quad_pred$fit)^2,na.rm=T))
rmse_Add_sea_Quad 
# 26.36

######################## Multiplicative Seasonality #########################

multi_sea_model<-lm(log_Passengers~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = train)
summary(multi_sea_model)
multi_sea_pred<-data.frame(predict(multi_sea_model,newdata=test,interval='predict'))
rmse_multi_sea<-sqrt(mean((test$Passengers-exp(multi_sea_pred$fit))^2,na.rm = T))
rmse_multi_sea 
# 140.06

######################## Multiplicative Seasonality Linear trend ##########################

multi_add_sea_model<-lm(log_Passengers~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = train)
summary(multi_add_sea_model) 
multi_add_sea_pred<-data.frame(predict(multi_add_sea_model,newdata=test,interval='predict'))
rmse_multi_add_sea<-sqrt(mean((test$Passengers-exp(multi_add_sea_pred$fit))^2,na.rm = T))
rmse_multi_add_sea
# 10.51

# Preparing table on model and it's RMSE values 

table_rmse<-data.frame(c("rmse_linear","rmse_expo","rmse_Quad","rmse_sea_add","rmse_Add_sea_Quad","rmse_multi_sea","rmse_multi_add_sea"),c(rmse_linear,rmse_expo,rmse_Quad,rmse_sea_add,rmse_Add_sea_Quad,rmse_multi_sea,rmse_multi_add_sea))
View(table_rmse)
colnames(table_rmse)<-c("model","RMSE")
View(table_rmse)

#Multiplicative Seasonality Linear trend has least RMSE ,so the best model will be
new_model <- lm(log_Passengers~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=AirlineData)
summary(new_model)

new_model_pred<-data.frame(predict(new_model,newdata=AirlineData,interval='predict'))
new_model_fin <- exp(new_model$fitted.values)
View(new_model_fin)

Month <- as.data.frame(AirlineData$Month)
Final <- as.data.frame(cbind(Month,AirlineData$Passengers, new_model_fin))
colnames(Final) <-c("Month","passengers","New_Pred_Value")

#actual graph 
plot(Final$passengers,main = "ActualGraph", xlab="passengers(Actual)", ylab="Months", col.axis="blue",type="o") 

#predicted graph
plot(Final$New_Pred_Value, main = "PredictedGraph", xlab="passengers(Predicted)", ylab="Months", col.axis="Green",type="s")

View(Final)# plot(Final$new_model_fin,type="o")
