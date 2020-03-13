# mlr
library(readr)
corolla <- read.csv(file.choose())
corolla<-corolla[c("Price","Age_08_04","KM","HP","cc","Doors","Gears","Quarterly_Tax","Weight")]
attach(corolla)
# Exploratory Data Analysis
# 1. Measures of Central Tendency
summary(corolla)

# 2. Measures of Dispersion
var(Price)
var(Age_08_04)
var(KM)
var(HP)
var(cc)
var(Doors)
var(Gears)
var(Quarterly_Tax)
var(Weight)

#std deviation
sd(Price)
sd(Age_08_04)
sd(KM)
sd(HP)
sd(cc)
sd(Doors)
sd(Gears)
sd(Quarterly_Tax)
sd(Weight)

# 3. Third Moment Business decision
library(moments)
skewness(Price)
skewness(Age_08_04)
skewness(KM)
skewness(HP)
skewness(cc)
skewness(Doors)
skewness(Gears)
skewness(Quarterly_Tax)
skewness(Weight)

#4. Fourth Moment Business decision
kurtosis(Price)
kurtosis(Age_08_04)
kurtosis(KM)
kurtosis(HP)
kurtosis(cc)
kurtosis(Doors)
kurtosis(Gears)
kurtosis(Quarterly_Tax)
kurtosis(Weight)
#6. Graphical representations
hist(Price)
#most of the cars under 5000 to 15000 range.
hist(Age_08_04)
# age of cars most 60 to 80.
hist(KM)
# KM Is most on 30000 to 15000 range.
hist(HP)
#MOST ofcars hp100 to 110range.
hist(cc)
hist(Doors)
hist(Gears)
hist(Quarterly_Tax)
hist(Weight)

plot(Price)
# there are 2-3 outliers
# all plots
pairs(corolla)
# price and age ,Km shows postive high corelation.
# age and km having high coliniarity which means there is multi coliniarity.

#correlation
cor(corolla)
# -0.87659050 here high negative significant r value for age and price ,which means price falls as Km increase.
#-0.56996016 high negative corelation between km and price.
#0.505672180 colinearity between km and age.

#partial corelation.
install.packages("corpcor")
library(corpcor)
cor2pcor(cor(corolla))
# there is no strong corelation between km and age.less multicolinerity.

#the linear model 
model.corolla <- lm(Price~.,data=corolla)
summary(model.corolla)

# Multiple R-squared:  0.8638 which means its a good model.std error values also less. pvalues expect cc and doors have high significance.
# Multicollinearity check
# Model based on only doors
model_corolla1 <- lm(Price~Doors)
summary(model_corolla1) # doors become significant

# Model based on only doors
model_corolla2<- lm(Price~cc)
summary(model_corolla2) 
# cc also become more signficant.

# Model based on doors and cc
model.corolla3<-lm(Price~Doors+cc,data=corolla)
summary(model.corolla3)
#both are significant.

#checking influence factors.
influence.measures(model.corolla)

library(car)
## plotting Influential measures 
windows()
influenceIndexPlot(model.corolla,id.n=3) # index plots for infuence measures
influencePlot(model.corolla,id.n=3) # A user friendly representation of the above

# Regression after deleting the 81 th observation, which is influential observation
model_inf<-lm(Price~.,data=corolla[-81,])
summary(model_inf)
#now cc  become significant.

# Regression after deleting the 81 ,222 th observation, which is influential observation
model_inf1<-lm(Price~.,data=corolla[-81,-222,])
summary(model_inf1)
#now cc  become more  significant.

## Added Variable plot to check correlation b/n variables and o/p variable
avPlots(model.corolla,id.n=2,id.cex=0.7)

# here doors have a straight line which indicates its have no impact on price ,so 
# removing the door variable and removing 81 th 222 data, creating final model.
final.model.corolla <- lm(Price~Age_08_04+KM+HP+cc+Gears+Quarterly_Tax+Weight,data = corolla[-81,-222,])
summary(final.model.corolla)

pred <- predict(final.model.corolla)
pred
final.model.corolla$residuals  #error
error <- sum(final.model.corolla$residuals)
error   # total error
rmse<-sqrt((sum(error)^2)/nrow(corolla))
rmse
# confidence intervel
confint(final.model.corolla,level = .95)
predict(final.model.corolla,interval ="predict")

# Multiple R-squared 0.8693 high, all p values become significant ,RMSE LESS standard error less.
#but confidence intervel are wider.
# to reduce confint intervel applying log transformation.
finalmodel_log=lm(Price~log(Age_08_04)+log(KM)+log(HP)+log(cc)+log(Gears)+log(Quarterly_Tax)+log(Weight), data = corolla[-81,-222,])
summary(finalmodel_log)
confint(finalmodel_log,level=0.95)
predict(finalmodel_log,interval="predict")

#Multiple R-squared:  0.841 decresed a bit,but confidence intervel are still wide 
#applying exponential transformation.

final.model.exp <- lm(log(Price)~Age_08_04+KM+HP+cc+Gears+Quarterly_Tax+Weight,data = corolla[-81,-222,])
summary(final.model.exp)
confint(final.model.exp,level=0.95)
predict(final.model.exp,interval="predict")

#checking multicolinarity 
vif(final.model.exp)
# all value less than 10 no multicolliniarity.

# Evaluate model LINE assumptions 
plot(final.model.exp)
# LINE assumptions has no viaolation.

#Multiple R-squared:  0.8522,confident intervel has decresed.all variables become more signficant with less p value
# the model will be 
   #corolla price= 8.497e+00 +-1.026e-02(age)+-1.876e-06(KM)+3.021e-03(HP)+-1.209e-04(CC)+5.628e-02(Gears)+8.413e-04(quarterly tax)+ 9.001e-04(weight)
       ## ans(exp)









