# predictive model for weight for calories consumed.
library(readr)
calory<- read.csv(file.choose())
View(calory)
# Exploratory data analysis 
summary(calory)
hist(calory$Weight.gained..grams.)
hist(calory$Calories.Consumed)
#Scatter plot
plot(calory$Weight.gained..grams., calory$Calories.Consumed)  # plot(X,Y)
attach(calory)
qqnorm(Weight.gained..grams.)
qqline(Weight.gained..grams.)
qqnorm(Calories.Consumed)
qqline(Calories.Consumed)
 # data are normally distributed
boxplot(Weight.gained..grams.,Calories.Consumed)

#Correlation Coefficient (r)

cor(Weight.gained..grams.,Calories.Consumed) 
#0.946991 which shows a high postive correlation.

#Simple Linear Regression model
reg <- lm(Weight.gained..grams. ~ Calories.Consumed)
summary(reg)
# Multiple R-squared:  0.8968,	Adjusted R-squared:  0.8882 
#a higher the r2 and values shows that model better fits the data.
#(Intercept)-625.75236 ,coefficient 0.42016

pred <- predict(reg)
pred
reg$residuals  #error
error <- sum(reg$residuals)
error # total error
rmse<-sqrt((sum(error)^2)/nrow(calory))
rmse
# confidence intervel
confint(reg,level = .95)
predict(reg,interval ="predict")


# SINCE R^ value is high,p value significant and RMSE values are less ,so the model will be

#(Intercept)-625.75236 +coefficient 0.42016 (calories in take )

# but the wider confidence intervel will reduce the accuracy of the model.


















