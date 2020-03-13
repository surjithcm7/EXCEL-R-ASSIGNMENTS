#glass data knn
glass=read.csv(file.choose())
colnames(glass)
# table of types.
table(glass$Type)

# recode type as a factor
glass$Type <- factor(glass$Type, levels = c("1", "2","3","4","5","6","7"),
                         labels = c("type1","type2","type3","type4","type5","type6","type7"))

# table or proportions with more informative labels
round(prop.table(table(glass$Type)) * 100, digits = 2)

# create normalization function
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
glass_n <- as.data.frame(lapply(glass[1:9], normalize))
# confirm that normalization worked
summary(glass_n$RI)

# create training and test data
library(caret)
Index <- createDataPartition(glass$Type, p=0.7, list=FALSE) 
#spliting the data into train and test using the ratio 7:3
glass_train <- glass_n[ Index,] 
glass_test <- glass_n[-Index,] 
glass_train_labels <- glass[ Index,10] 
glass_test_labels <- glass[-Index,10] 

table(glass_train_labels)
table(glass_test_labels) 


#training model on data
library(class)

glass_test_pred <- knn(train = glass_train, test = glass_test,
                      cl = glass_train_labels, k=10)

table(glass_test_pred,glass_test_labels)
mean(glass_test_pred==glass_test_labels)
#accuracy is 0.60.
##--------Evaluating model performance ----

# load the "gmodels" library
library(gmodels)

# Create the cross tabulation of predicted vs. actual
CrossTable(x = glass_test_labels, y = glass_test_pred,
           prop.chisq=FALSE)

#now we try for different k values to check whether above model is of high accuracy
glass_pred1 <- knn(train = glass_train , test = glass_test, cl = glass_train_labels, k=13)
CrossTable( x =  glass_test_labels, y = glass_pred1)
mean(glass_pred1==glass_test_labels)
#0.5081967 accuracy is less.

glass_pred2 <- knn(train = glass_train , test = glass_test, cl = glass_train_labels, k=12)
CrossTable( x =  glass_test_labels, y = glass_pred2)
mean(glass_pred2==glass_test_labels)
#0.54 % so the accuracy is less.

glass_pred3 <- knn(train = glass_train , test = glass_test, cl = glass_train_labels, k=9)
CrossTable( x =  glass_test_labels, y = glass_pred3)
mean(glass_pred3==glass_test_labels)
# 0.57 % accuracy is so less.

# so the highest 60% on k value 10 .