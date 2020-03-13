library(readr)
delivery <- read.csv(file.choose())
 # EDA
summary(delivery)
attach(delivery)
hist(Delivery.Time)
hist(Sorting.Time)
boxplot(Delivery.Time)
boxplot(Sorting.Time)
library(moments)
kurtosis(Delivery.Time)
kurtosis(Sorting.Time)
# platokurtic
skewness(Delivery.Time)
skewness(Sorting.Time)
# since the values are close to zero which shows a normal distribution of data.
qqnorm(Delivery.Time)
qqline(Delivery.Time)
qqnorm(Sorting.Time)
qqline(Sorting.Time)
cor(Sorting.Time,Delivery.Time)
# r value is 0.825 which shows a moderate postive corelation and relationship between variables.

reg <- lm(Delivery.Time ~ Sorting.Time)
summary(reg)
pred <- predict(reg)
pred
reg$residuals  #error
error <- sum(reg$residuals)
error # total error
rmse<-sqrt((sum(error)^2)/nrow(delivery))
rmse
# confidence intervel
confint(reg,level = .95)
predict(reg,interval ="predict")

# r^ 0.68 which is low so model is not the best.
#pvalue is significant but that can not be considerd.
#so we have to increse r^ value by model transformation 

# ggplot for adding regresion line for data
library(ggplot2)



ggplot(data = delivery, aes(x = Sorting.Time, y =Delivery.Time)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = delivery, aes(x=Sorting.Time, y=pred))

# changing to logrthamic transformation.
# x = log(sorting time); y = delivery time

plot(log(Sorting.Time), Delivery.Time)

cor(log(Sorting.Time), Delivery.Time)
#r value has slightly incresesd
reg_log <- lm(Delivery.Time~ log(Sorting.Time))   # lm(Y ~ X)
summary(reg_log)
pred1 <- predict(reg_log)
pred1
reg_log$residuals  #error
error1 <- sum(reg_log$residuals)
error1 # total error
rmse1<-sqrt((sum(error1)^2)/nrow(delivery))
rmse1
# confidence intervel
confint(reg_log,level = .95)
predict(reg_log,interval ="predict")

#r^2 value is 0.69 which has nomuch increse ,but predicted values are good and intervels are very less.
## Exponential Model
# x = (sorting time); y = log (delivery time)


plot (Sorting.Time, log(Delivery.Time))

cor(Sorting.Time, log(Delivery.Time))
# 0.8431773 r value has incresed.
reg_exp <- lm(log(Delivery.Time)~ Sorting.Time)   # lm(Y ~ X)
summary(reg_exp)
pred2 <- predict(reg_exp)
pred2
reg_exp$residuals  #error
error2 <- sum(reg_exp$residuals)
error2 # total error
rmse2<-sqrt((sum(error2)^2)/nrow(delivery))
rmse2
# confidence intervel
confint(reg_exp,level = .95)
predict(reg_exp,interval ="predict")

#0.7109,multiple r^ value incresed and prediction values shows same,rmse is less.
# so the model is delivery time = 2.12137+0.10555*(sorting time) = exp(ans)












