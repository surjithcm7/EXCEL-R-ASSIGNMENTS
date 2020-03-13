library(readr)
emp <- read.csv(file.choose())
# EDA
summary(emp)
attach(emp)
hist(Salary_hike)
hist(Churn_out_rate)
boxplot(Salary_hike)
boxplot(Churn_out_rate)
qqnorm(Salary_hike)
qqline(Salary_hike)
qqnorm(Churn_out_rate)
qqline(Churn_out_rate)

#all values are normally distributed.
cor(Salary_hike,Churn_out_rate)
#the correlation value is -0.9117216 ,highly negatively corelated.shows inverse realtionship of x and y.
reg <- lm(Churn_out_rate~Salary_hike)
summary(reg)
pred <- predict(reg)
pred
reg$residuals  #error
error <- sum(reg$residuals)
error   # total error
rmse<-sqrt((sum(error)^2)/nrow(emp))
rmse
# confidence intervel
confint(reg,level = .95)
predict(reg,interval ="predict")

#Multiple R-squared:  0.8312, p value significant,rmse is less and
#confidence intervel is not wide.std error is high.


## changing to logrthamic transformation.
# x = log(salary hike); y = churn out rate


cor(log(Salary_hike), Churn_out_rate)
#r value has slightly incresesd,-0.9212077
reg_log <- lm(Churn_out_rate~ log(Salary_hike))   # lm(Y ~ X)
summary(reg_log)
pred1 <- predict(reg_log)
pred1
reg_log$residuals  #error
error1 <- sum(reg_log$residuals)
error1 # total error
rmse1<-sqrt((sum(error1)^2)/nrow(emp))
rmse1
# confidence intervel
confint(reg_log,level = .95)
predict(reg_log,interval ="predict")
# Multiple R-squared:  0.8486 incresed ,rmse low but std error is high , 

## Exponential Model
#x = salary hike; y =log( churn out rate)



cor(Salary_hike, log(Churn_out_rate))
# -0.9346361 r value has incresed.
reg_exp <- lm(log(Churn_out_rate)~ Salary_hike)   # lm(Y ~ X)
summary(reg_exp)
pred2 <- predict(reg_exp)
pred2
reg_exp$residuals  #error
error2 <- sum(reg_exp$residuals)
error2 # total error
rmse2<-sqrt((sum(error2)^2)/nrow(emp))
rmse2
# confidence intervel
confint(reg_exp,level = .95)
predict(reg_exp,interval ="predict")

#0.8735,multiple r^ value incresed and prediction values shows same,rmse is less. std error has decresed.
# so the model is churn out rate = 6.6383000+-0.0013963(salary hike) = exp(ans)






