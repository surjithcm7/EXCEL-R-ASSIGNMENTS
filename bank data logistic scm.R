#logistic regression on bank data

# Import banks dataset after cleaning it and seperating it into correct columns
banks <- read.csv(choose.files()) 
View(banks)

sum(is.na(banks))
# Checking for NA values in the dataset. No NA values found, so no need top remove them
 
attach(banks)
colnames(banks)
str(banks)
summary(banks) 

# Changing values for caterigorical features
library(plyr)
banks$marital <- as.factor(revalue(banks$marital, c("single" = 1, "married" = 2, "divorced" = 3)))
banks$education <- as.factor(revalue(banks$education, c("unknown" = 0, "primary" = 1, "secondary" = 2, "tertiary" = 3)))
banks$default <- as.factor(revalue(banks$default, c("yes" = 1, "no" = 0)))
banks$housing <- as.factor(revalue(banks$housing, c("yes" = 1, "no" = 0)))
banks$loan <- as.factor(revalue(banks$loan, c("yes" = 1, "no" = 0)))
banks$contact <- as.factor(revalue(banks$contact, c("unknown" = 0, "telephone" = 1, "cellular" = 2)))
banks$month <- as.factor(revalue(banks$month, c("jan" = 1, "feb" = 2, "mar" = 3, "apr" = 4, "may" = 5, "jun" = 6, "jul" = 7, "aug" = 8, "sep" = 9, "oct" = 10, "nov" = 11, "dec" = 12)))
banks$poutcome <- as.factor(revalue(banks$poutcome, c("failure" = -1, "unknown" = 0, "success" = 1, "other" = 2)))
banks$y <- as.factor(revalue(banks$y, c("yes" = 1, "no" = 0)))
View(banks)


# Taking all independent variables except "job description" for building model
banks_main <- banks[,-2] 
View(banks_main)

model <- glm(y~., data=banks_main, family = "binomial") # Building Model based on all independent variables except "job description"
 
#odds ratio\ Exponential of Coefficient of the built model

exp(coef(model))
prob <- predict(model, banks_main, type="response")
summary(model)

# Confusion Matrix based on threshold greater than 0.5
confusion <- table(prob>0.5, banks_main$y) 
confusion

# Accuracy of the built model
accuracy <- sum(diag(confusion)/sum(confusion)) 
accuracy 
# 90% Accurate model

