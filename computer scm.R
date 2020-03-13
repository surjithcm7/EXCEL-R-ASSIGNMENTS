library(readr)
comp_data <- read.csv(file.choose())

View(comp_data)
# removing unwanted colum no
comp_data <- comp_data[-1]


attach(comp_data)
summary(comp_data)

#some variables are  character converting to dummy variable.
library(plyr)
comp_data$cd <- as.factor(revalue(comp_data$cd, c("yes" = 1, "no" = 0)))
comp_data$multi <- as.factor(revalue(comp_data$multi,c("yes" = 1, "no" = 0)))
comp_data$premium<- as.factor(revalue(comp_data$premium,c("yes" = 1, "no" = 0)))
View(comp_data)


#EDA
#price
hist(price)
##skewness and kurtosis
library(moments)
skewness(price)
kurtosis(price)
boxplot(price)
#the data is posirively skewed and lepto kurtic or having higher peak than the normal
#there are some outliers

#speed
hist(speed)
skewness(speed)
kurtosis(speed)
boxplot(speed)
#data is positively skewed and havimg lesser peak than the normal data

#hd
hist(hd)
skewness(hd)
kurtosis(hd)
boxplot(hd)
#data is positively skewed and high peak compared to the normal data
#there are some outliers in the data

#ram
hist(ram)
skewness(ram)
kurtosis(ram)
boxplot(ram)
#here the data is positively skewed and having higher peak there are some outliers which shows 
#there are some computers with having high ram like 16 gb, 24 gb and 32 gb
#most common ram capacity is 8 gb which is represented by median

#checking normality
qqnorm(price)
qqline(price)

#normally distributed.

qqnorm(speed)
qqline(speed)
# no normally distribution

qqnorm(hd)
qqline(hd)
#no normally distribution

qqnorm(ram)
qqline(ram)
#no normally distribution.

qqnorm(screen)
qqline(screen)
#no normally distribution.

#since many data are rigid type so normalization
norm <- function(x)
  
{
  return((x-min(x))/(max(x)-min(x)))
}

normdata <- as.data.frame(lapply(comp_data[0:5],norm))
View(normdata)
data <- cbind(cd,multi,premium,ads,trend,normdata)
data <- data.frame(data)

library(plyr)
data$cd <- as.factor(revalue(comp_data$cd, c("yes" = 1, "no" = 0)))
data$multi <- as.factor(revalue(comp_data$multi,c("yes" = 1, "no" = 0)))
data$premium<- as.factor(revalue(comp_data$premium,c("yes" = 1, "no" = 0)))

View(data)
attach(data)
summary(data)
pairs(data)
cor(normdata)
# 0.62 correlation between ram and price 

#partial corelation.
library(corpcor)
cor2pcor(cor(normdata))
##here we can see that the price is depending mainly on the ram size 

# hd and ram multicolinerity.
#model
model <- lm(price~.,data=data)
summary(model)
# all the variables have significant pvalues ,error values are also less and 
#Multiple R-squared:  0.7756 ,goodmodel.

#multicollinerity
vif(model)
#all value less than 10 ,no multicollinerity.

#checking influence by vif
influence.measures(model)

## plotting Influential measures 
library(car)
## plotting Influential measures 
windows()
influenceIndexPlot(model,id.n=3) # index plots for infuence measures
influencePlot(model,id.n=3) # A user friendly representation of the above

# data 1441,1701 have high influence on model
#rebuilding model after omitting these values.
modelfinal <- lm (price~.,data = data[-c(1441,1701),])
summary(modelfinal)
# Multiple R-squared:  0.7777 value incresed ,std error is less  p values are signficant.

pred <- predict(modelfinal)
pred
modelfinal$residuals  #error
error <- sum(modelfinal$residuals)
error   # total error
rmse<-sqrt((sum(error)^2)/nrow(comp_data))
rmse
# confidence intervel
confint(modelfinal,level = .95)
predict(modelfinal,interval ="predict")

#confident intervel are less and RMSE are less the model will be.

##price= 3.315e-01+3.271e-01(ram)+1.359e-02(cd)+2.354e-02(multi)+1.146e-01(premium)+ 1.463e-04(ads)+-1.161e-02(trend)+
# 1.567e-01(speed)+ 3.517e-01(hd)+8.164e-02(screen)



# Evaluate model LINE assumptions 
plot(modelfinal)
# LINE assumptions has no viaolation.










