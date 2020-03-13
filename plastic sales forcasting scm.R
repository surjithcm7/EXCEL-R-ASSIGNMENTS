#forcasting plastic sales

library(forecast)
library(fpp)
library(smooth)
library(readxl)


Plastic<-read.csv(file.choose()) 
View(Plastic) # Seasonality 12 months 
windows()
plot(Plastic$Sales,type="o") #time plot


# So creating 12 dummy variables 
X<- data.frame(outer(rep(month.abb,length = 60), month.abb,"==") + 0 )# Creating dummies for 12 months
View(X)
colnames(X)<-month.abb # Assigning month names 
View(X)
Plasticdata<-cbind(Plastic,X)
View(Plasticdata)
colnames(Plasticdata)

#input t
Plasticdata["t"]<- 1:60
View(Plasticdata)

#adding log and t^2 values.
Plasticdata["log_Sales"]<-log(Plasticdata["Sales"])
Plasticdata["t_square"]<-Plasticdata["t"]*Plasticdata["t"]
attach(Plasticdata)

# partitioning
train<-Plasticdata[1:48,]
test<-Plasticdata[49:60,]

########################### LINEAR MODEL #############################
linear_model<-lm(Sales~t,data=train)
summary(linear_model)
linear_pred<-data.frame(predict(linear_model,interval='predict',newdata =test))
View(linear_pred)
rmse_linear<-sqrt(mean((test$Sales-linear_pred$fit)^2,na.rm = T))
rmse_linear
# 260.9378 


######################### Exponential #################################
expo_model<-lm(log_Sales~t,data=train)
summary(expo_model)
expo_pred<-data.frame(predict(expo_model,interval='predict',newdata=test))
rmse_expo<-sqrt(mean((test$Sales-exp(expo_pred$fit))^2,na.rm = T))
rmse_expo 
# 268.693


######################### Quadratic ####################################
Quad_model<-lm(Sales~t+t_square,data=train)
summary(Quad_model)
Quad_pred<-data.frame(predict(Quad_model,interval='predict',newdata=test))
rmse_Quad<-sqrt(mean((test$Sales-Quad_pred$fit)^2,na.rm=T))
rmse_Quad 
# 297.4067 

######################### Additive Seasonality #########################
sea_add_model<-lm(Sales~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov+Dec,data=train)
summary(sea_add_model)
sea_add_pred<-data.frame(predict(sea_add_model,newdata=test,interval='predict'))
rmse_sea_add<-sqrt(mean((test$Sales-sea_add_pred$fit)^2,na.rm = T))
rmse_sea_add 
# 235.6027 


######################## Additive Seasonality with Linear #################
Add_sea_Linear_model<-lm(Sales~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov+Dec,data=train)
summary(Add_sea_Linear_model)
Add_sea_Linear_pred<-data.frame(predict(Add_sea_Linear_model,interval='predict',newdata=test))
rmse_Add_sea_Linear<-sqrt(mean((test$Sales-Add_sea_Linear_pred$fit)^2,na.rm=T))
rmse_Add_sea_Linear # 135.5536 


######################## Additive Seasonality with Quadratic #################
Add_sea_Quad_model<-lm(Sales~t+t_square+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov+Dec,data=train)
summary(Add_sea_Quad_model)
Add_sea_Quad_pred<-data.frame(predict(Add_sea_Quad_model,interval='predict',newdata=test))
rmse_Add_sea_Quad<-sqrt(mean((test$Sales-Add_sea_Quad_pred$fit)^2,na.rm=T))
rmse_Add_sea_Quad 
# 218.1939 

######################## Multiplicative Seasonality #########################
multi_sea_model<-lm(log_Sales~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov+Dec,data = train)
summary(multi_sea_model)
multi_sea_pred<-data.frame(predict(multi_sea_model,newdata=test,interval='predict'))
rmse_multi_sea<-sqrt(mean((test$Sales-exp(multi_sea_pred$fit))^2,na.rm = T))
rmse_multi_sea
# 239.6543


######################## Multiplicative Seasonality Linear trend ##########################
multi_add_sea_model<-lm(log_Sales~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov+Dec,data = train)
summary(multi_add_sea_model)
multi_add_sea_pred<-data.frame(predict(multi_add_sea_model,newdata=test,interval='predict'))
rmse_multi_add_sea<-sqrt(mean((test$Sales-exp(multi_add_sea_pred$fit))^2,na.rm = T))
rmse_multi_add_sea 
# 160.6833


# Preparing table on model and it's RMSE values 
table_rmse<-data.frame(c("rmse_linear","rmse_expo","rmse_Quad","rmse_sea_add","rmse_Add_sea_Quad","rmse_multi_sea","rmse_multi_add_sea"),c(rmse_linear,rmse_expo,rmse_Quad,rmse_sea_add,rmse_Add_sea_Quad,rmse_multi_sea,rmse_multi_add_sea))
View(table_rmse)
colnames(table_rmse)<-c("model","RMSE")
View(table_rmse)


# Multiplicative Seasonality with  Linear trend  has least RMSE value,so the best model will be
new_model<-lm(log_Sales~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov+Dec,data = Plasticdata)
new_model_pred<-data.frame(predict(new_model,newdata=Plasticdata,interval='predict'))
new_model_fin <- exp(new_model$fitted.values)
View(new_model_fin)

Month <- as.data.frame(Plasticdata$Month)
Final <- as.data.frame(cbind(Month,Plasticdata$Sales, new_model_fin))
colnames(Final) <-c("Month","Sales","New_Pred_Value")

#actual graph 
plot(Final$Sales,main = "ActualGraph", xlab="Sales(Actual)", ylab="Months", col.axis="blue",type="o") 

#predicted graph
plot(Final$New_Pred_Value, main = "PredictedGraph", xlab="Sales(Predicted)", ylab="Months", col.axis="Green",type="s")

View(Final)# plot(Final$new_model_fin,type="o")


