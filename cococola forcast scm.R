#cococola forcasting
library(readxl)
library(forecast)
library(fpp)
library(smooth)

CocaCola<-read_excel(file.choose()) # read the CocaCola Sales data
View(CocaCola)# seasonality with 4 quarters 

plot(CocaCola$Sales,type="o")

#Creating 4 Dummy variables for 4 quarters

Q1 <-  ifelse(grepl("Q1",CocaCola$Quarter),'1','0')
Q2 <-  ifelse(grepl("Q2",CocaCola$Quarter),'1','0')
Q3 <-  ifelse(grepl("Q3",CocaCola$Quarter),'1','0')
Q4 <-  ifelse(grepl("Q4",CocaCola$Quarter),'1','0')

#combining dummy variables 
CocaColaData<-cbind(CocaCola,Q1,Q2,Q3,Q4)
View(CocaColaData)
colnames(CocaColaData)

#input t
CocaColaData["t"]<- 1:42 
View(CocaColaData)
#adding log and t^2 values
CocaColaData["log_sales"]<-log(CocaColaData["Sales"])
CocaColaData["t_square"]<-CocaColaData["t"]*CocaColaData["t"]
attach(CocaColaData)

#data partition
library(caret)
part<-createDataPartition(CocaColaData$Sales,p=0.7,list=F)
train<-CocaColaData[part,]
test<-CocaColaData[-part,]
View(train)
View(test)



##################################LINEAR MODEL #############################

linear_model<-lm(Sales~t,data=train)
summary(linear_model)
linear_pred<-data.frame(predict(linear_model,interval='predict',newdata =test))  #predicting a model on test data
View(linear_pred)
rmse_linear<-sqrt(mean((test$Sales-linear_pred$fit)^2,na.rm = T)) #na.rm=T means removing any na values
rmse_linear
##307.278


######################### Exponential #################################

expo_model<-lm(log_sales~t,data=train)
summary(expo_model)
expo_pred<-data.frame(predict(expo_model,interval='predict',newdata=test))
rmse_expo<-sqrt(mean((test$Sales-exp(expo_pred$fit))^2,na.rm = T))
rmse_expo
# 263.62


######################### Quadratic ####################################

Quad_model<-lm(Sales~t+t_square,data=train)
summary(Quad_model)
Quad_pred<-data.frame(predict(Quad_model,interval='predict',newdata=test))
rmse_Quad<-sqrt(mean((test$Sales-Quad_pred$fit)^2,na.rm=T))
rmse_Quad 
# 314.03


######################### Additive Seasonality #########################

sea_add_model<-lm(Sales~Q1+Q2+Q3+Q4,data=train)
summary(sea_add_model)
sea_add_pred<-data.frame(predict(sea_add_model,newdata=test,interval='predict'))
rmse_sea_add<-sqrt(mean((test$Sales-sea_add_pred$fit)^2,na.rm = T))
rmse_sea_add
# 944.70

######################## Additive Seasonality with Linear #################

Add_sea_Linear_model<-lm(Sales~t+Q1+Q2+Q3+Q4,data=train)
summary(Add_sea_Linear_model)
Add_sea_Linear_pred<-data.frame(predict(Add_sea_Linear_model,interval='predict',newdata=test))
rmse_Add_sea_Linear<-sqrt(mean((test$Sales-Add_sea_Linear_pred$fit)^2,na.rm=T))
rmse_Add_sea_Linear
# 283.305

######################## Additive Seasonality with Quadratic #################

Add_sea_Quad_model<-lm(Sales~t+t_square+Q1+Q2+Q3+Q4,data=train)
summary(Add_sea_Quad_model)
Add_sea_Quad_pred<-data.frame(predict(Add_sea_Quad_model,interval='predict',newdata=test))
rmse_Add_sea_Quad<-sqrt(mean((test$Sales-Add_sea_Quad_pred$fit)^2,na.rm=T))
rmse_Add_sea_Quad 
#155.66 

######################## Multiplicative Seasonality #########################

multi_sea_model<-lm(log_sales~Q1+Q2+Q3+Q4,data = train)
summary(multi_sea_model)
multi_sea_pred<-data.frame(predict(multi_sea_model,newdata=test,interval='predict'))
rmse_multi_sea<-sqrt(mean((test$Sales-exp(multi_sea_pred$fit))^2,na.rm = T))
rmse_multi_sea
#941.68

######################## Multiplicative Seasonality Linear trend ##########################

multi_add_sea_model<-lm(log_sales~t+Q1+Q2+Q3+Q4,data = train)
summary(multi_add_sea_model) 
multi_add_sea_pred<-data.frame(predict(multi_add_sea_model,newdata=test,interval='predict'))
rmse_multi_add_sea<-sqrt(mean((test$Sales-exp(multi_add_sea_pred$fit))^2,na.rm = T))
rmse_multi_add_sea 
#149.60

# Preparing table on model and it's RMSE values 

table_rmse<-data.frame(c("rmse_linear","rmse_expo","rmse_Quad","rmse_sea_add","rmse_Add_sea_Quad","rmse_multi_sea","rmse_multi_add_sea"),c(rmse_linear,rmse_expo,rmse_Quad,rmse_sea_add,rmse_Add_sea_Quad,rmse_multi_sea,rmse_multi_add_sea))
View(table_rmse)
colnames(table_rmse)<-c("model","RMSE")
View(table_rmse)

# Multiplicative Seasonality with  Linear trend has least RMSE value 149.6,so the best model will be

final_model <- lm(log_sales~t+Q1+Q2+Q3+Q4,data=CocaColaData)
final_model_pred<-data.frame(predict(final_model,newdata=CocaColaData,interval='predict'))

final_model_fin <- final_model$fitted.values

View(final_model_fin)

Quarter <- as.data.frame(CocaColaData$Quarter)

Final <- as.data.frame(cbind(Quarter,CocaColaData$Sales,final_model_fin))
colnames(Final) <-c("Quarter","Sales","New_Pred_Value")

#actualgraph
plot(Final$Sales,main = "ActualGraph", xlab="Sales(Actual)", ylab="Quarter",
     col.axis="blue",type="o")
#predicted graph
plot(Final$New_Pred_Value, main = "PredictedGraph", xlab="Sales(Predicted)", ylab="Quarter",
     col.axis="Red",type="s")

View(Final)





