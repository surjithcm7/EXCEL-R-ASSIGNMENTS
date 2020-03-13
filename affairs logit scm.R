##install.packages('AER')

data(Affairs,package="AER")
dim(Affairs)
View(Affairs)
colnames(Affairs)
str(Affairs)


Affairs$affairs =as.factor(Affairs$affairs)
summary(Affairs$affairs)
Affairs$gender = factor(Affairs$gender,levels = c('female', 'male'),labels = c(0, 1))
Affairs$children = factor(Affairs$children,levels = c('no', 'yes'),labels = c(0, 1))
View(Affairs)


logit = glm(affairs ~ factor(gender)  + age + yearsmarried 
            + factor(children) + factor(religiousness) + factor(education) + factor(occupation) + factor(rating),family= "binomial",data=Affairs)
logit
summary(logit)

# Odds Ratio
exp(coef(logit))

# Confusion Matrix Table

prob=predict(logit,type=c("response"),Affairs)
prob

confusion<-table(prob>0.6,Affairs$affairs)
confusion


# Model Accuracy

Accuracy<-sum(diag(confusion))/sum(confusion)
Accuracy
#74% accurate.
