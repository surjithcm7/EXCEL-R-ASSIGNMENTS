#MLR on 50 startup
library(readr)
startup <- read.csv(file.choose())
View(startup)
startup <- startup[c("R.D.Spend","Administration","Profit","Marketing.Spend")]
summary(startup)
attach(startup)
#measure of dispersion

var(R.D.Spend)

var(Administration)
var(Marketing.Spend)
var(Profit)
sd(R.D.Spend)
sd(Administration)
sd(Marketing.Spend)
sd(Profit)

#skewness 
library(moments)
skewness(R.D.Spend)
skewness(Administration)
skewness(Marketing.Spend)
skewness(Profit)

kurtosis(R.D.Spend)
kurtosis(Administration)
kurtosis(Profit)
kurtosis(Marketing.Spend)

#6. Graphical representations
hist(R.D.Spend)
hist(Administration)
hist(Profit)
hist(Marketing.Spend)

#checking normality
qqnorm(R.D.Spend)
qqline(R.D.Spend)
#normally distributed.

qqnorm(Administration)
qqline(Administration)
#normally distributed.

qqnorm(Marketing.Spend)
qqline(Marketing.Spend)
#normally distributed.
qqnorm(Profit)
qqline(Profit)
#normally distributed.

# all plots
pairs(startup)
   # analyzing the scatter plot ,it shows
   #1 <- profit &r.d spend high postive correlation.
   #2 <- profit and mrketing spend have good postive correlation.
   #3 <- multicolinerity on independent variables marketing and r.d spend.


cor(startup)
# 0.9729005 profit and R.D.Spend have a high postive correlation.
#0.7477657 profit and Marketing.Spend have a moderate postive correlation.
#0.72424813 Marketing.Spend and R.D.Spend have collinerity.

#partial corelation.
library(corpcor)
cor2pcor(cor(startup))
# 0.03890336 Marketing.Spend and R.D.Spend  collinerity has reduced.

#The linear model 
model.startup <- lm(Profit~.,data=startup)
summary(model.startup)

#here p-values are not significant for administration and marketing spend.
#so we rebuild the model using only one variable.
model2 <- lm(Profit~R.D.Spend,data = startup)
summary(model2)

#here Multiple R-squared:  0.9465 high znd  p value is significant less standard error and high r^2 value
#now we build model using marketin spend variable

model3 <- lm(Profit~Marketing.Spend,data = startup)
summary(model3)

#here p values are significant
#now we build model using 2 variables-marketing spend and administration

model4<- lm(Profit~Administration+Marketing.Spend,data = startup)
summary(model4)

#here p value is not significant for intercept,
# so we go forinfluence index plot.

influence.measures(model.startup)
## plotting Influential measures 
library(car)
## plotting Influential measures 
windows()
influenceIndexPlot(model.startup,id.n=5) # index plots for infuence measures
influencePlot(model.startup,id.n=5) # A user friendly representation of the above

#here by checking the plots ,46,49,50,47 have more influence on factors
#removing those and building new model.
model.startup1<-lm(Profit~.,data=startup[-c(46,50,49,47)])
summary(model.startup1)

#Multiple R-squared:  0.9507,pvalues for administration and marketing spend is not significant.

#checking multi colinerity bt vif
vif(model.startup)
#all values are less than 10,no multicollinerity.

## Added Variable plot to check correlation b/n variables and o/p variable
avPlots(model.startup1,id.n=2,id.cex=0.7)

# here administaration  have a straight line which indicates its have no impact on profit. ,so 
# removing adminsrative variable and creating new model after deleting 46,50,49,47 rows

model.startup1<-lm(Profit~.-Administration,data=startup[-c(46,50,49,47)])
summary(model.startup1)

# Marketing.Spend is not significant ,so trnasformation to log model.
##log transformation.removing infinity values also.
model.final.log <-lm(Profit~log(R.D.Spend)+log(Marketing.Spend),data=startup[-c(46,50,49,47,48,20),])
summary(model.final.log)
# marketing spend is insignificant 
#exponential transformation.

model.final.exp <-lm(log(Profit)~R.D.Spend +Marketing.Spend,data=startup[-c(46,50,49,47,48,20),])
summary(model.final.exp)

# now model become significant with Multiple R-squared:  0.9541 and variables become significant.
pred <- predict(model.final.exp)

pred
model.final.exp$residuals  #error
error <- sum(model.final.exp$residuals)
error   # total error
rmse<-sqrt((sum(error)^2)/nrow(startup))
rmse
# confidence intervel
confint(model.final.exp,level = .95)
predict(model.final.exp,interval ="predict")

#now variables significant,high multiple r^ ,less std.error,less RMSE values this will be the best model with lesser cofident intervel.
# final model     
    # profit= 1.108e+01+6.143e-06(R.D.Spend)+2.901e-07(Marketing.Spend)=Ans(exponential)


# Evaluate model LINE assumptions 
plot(model.final.exp)
# LINE assumptions has no viaolation.








