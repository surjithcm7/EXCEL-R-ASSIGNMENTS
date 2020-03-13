library(readr)
sal <- read.csv(file.choose())
#eda
summary(sal)
attach(sal)
hist(Salary)
hist(YearsExperience)
boxplot(Salary)
boxplot(YearsExperience)
library(moments)
skewness(Salary)
skewness(YearsExperience)
kurtosis(Salary)
kurtosis(YearsExperience)
#salary data ,experience is positively skewed
#platykurtic forsalary and experience.
qqnorm(Salary)
qqline(Salary)
qqnorm(YearsExperience)
qqline(YearsExperience)
#normal distribution on salary and years of experience data.

cor(Salary,YearsExperience)
#0.9782416  which shows high postive corelation.
reg <- lm(Salary ~ YearsExperience)
summary(reg)
pred <- predict(reg)
pred
reg$residuals  #error
error <- sum(reg$residuals)
error # total error
rmse<-sqrt((sum(error)^2)/nrow(sal))
rmse
# confidence intervel
confint(reg,level = .95)
predict(reg,interval ="predict")
#Multiple R-squared:  0.957 ,which means model is best ,errors are less ,rmse less but std error values are too high.
#so we have to increse r^ value by model transformation

# changing to logrthamic transformation.
# x = log(years of experience); y = salary

plot(log(YearsExperience), Salary)

cor(log(YearsExperience), Salary)
#r value has slightly decresesd 0.9240611
reg_log <- lm(Salary~ log(YearsExperience))   # lm(Y ~ X)
summary(reg_log)
pred1 <- predict(reg_log)
pred1
reg_log$residuals  #error
error1 <- sum(reg_log$residuals)
error1 # total error
rmse1<-sqrt((sum(error1)^2)/nrow(sal))
rmse1
# confidence intervel
confint(reg_log,level = .95)
predict(reg_log,interval ="predict")
# Multiple R-squared:  0.8539 reduced and high std error.

## Exponential Model
# x = (years of experince); y = log (salary)


cor(YearsExperience, log(Salary))
# 0.9653844 r value has incresed.

reg_exp <- lm(log(Salary)~ YearsExperience)   # lm(Y ~ X)
summary(reg_exp)
pred2 <- predict(reg_exp)
pred2
reg_exp$residuals  #error
error2 <- sum(reg_exp$residuals)
error2 # total error
rmse2<-sqrt((sum(error2)^2)/nrow(sal))
rmse2
# confidence intervel
confint(reg_exp,level = .95)
predict(reg_exp,interval ="predict")

#Multiple R-squared:  0.932,multiple r^ value incresed and prediction values shows same,rmse is less.
#confidence interval decresed,std error are less.so this is best model as far.
#model will be salary=10.507402 +0.125453(years of experince)=exp(answer)




















