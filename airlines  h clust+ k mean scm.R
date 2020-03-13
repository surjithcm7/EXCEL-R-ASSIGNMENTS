#airlines
library(readr)
airlines <- read.csv(file.choose())
View(airlines)

summary(airlines)
# we can see many outliers in given data
#checking normality
attach(airlines)
qqnorm(Balance)
qqline(Balance)
boxplot(Balance)
#not normal distribution.and we can see many outliers on data.
qqnorm(Qual_miles)
qqline(Qual_miles)
boxplot(Qual_miles)
# data is not normal and some columns are zero and some are not.too much outliers for data.

qqnorm(cc1_miles)
qqline(cc1_miles)
boxplot(Qual_miles)
#data are normal and no outliers.
qqnorm(Bonus_miles)
qqline(Bonus_miles)
boxplot(Bonus_miles)
#data not follows normal distribution and there are so much outliers.
qqnorm(Bonus_trans)
qqline(Bonus_trans)
boxplot(Bonus_trans)
#data follows normal distribution but there are so much outliers.
qqnorm(Flight_miles_12mo)
qqline(Flight_miles_12mo)
boxplot(Flight_miles_12mo)
#data not follows normal distribution and there are so much outliers.

qqnorm(Flight_trans_12)
qqline(Flight_trans_12)
boxplot(Flight_trans_12)
#data not follows normal distribution and there are so much outliers

qqnorm(Days_since_enroll)
qqline(Days_since_enroll)
boxplot(Days_since_enroll)
#data follows normal distribution and there  no outliers

#datas are in several format so we normalize the data.

normdata<-scale(airlines[,2:(ncol(airlines))]) 
View(normdata)
d<-dist(normdata,method="euclidean")
fit<-hclust(d,method="complete")
plot(fit)
plot(fit,hang=-1)
rect.hclust(fit,k=10,border="red")
groups<-cutree(fit,k=10)
membership<-as.matrix(groups)
final<-data.frame(airlines,membership)
View(final)

#using single linkage
fit1<-hclust(d,method="single")
plot(fit1)
plot(fit1,hang=-1)
rect.hclust(fit1,k=10,border="red")
groups1<-cutree(fit1,k=10)
membership1<-as.matrix(groups1)
final1<-data.frame(airline,membership1)
View(final1)

#since there are 3999 observations ,it is not appropriate to perform hierarchieal clustering
#because the dendrogram representation is not clear 
#so we go for k means clustering
library(plyr)
wss = NULL

kmean1<- kmeans(normdata,10)
str(kmean1)
install.packages('animation')
library(animation)
kmeans.ani(airlines,10)
kmeans.ani(normdata,10)
wss = (nrow(normdata)-1)*sum(apply(normdata, 2, var))
for (i in 2:10) wss[i] = sum(kmeans(normdata, centers=i)$withinss)
plot(1:10, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")   # Look for an "elbow" in the scree plot #
title(sub = "K-Means Clustering Scree-Plot")

twss <- NULL
for (i in 2:15){
  twss <- c(twss,kmeans(normdata,i)$tot.withinss)
}
twss
plot(2:15,twss,type="o")
kmean6 <- kmeans(normdata,6)
str(kmean6)
str(kmean6)
kmean11<-kmeans(normdata,11)
str(kmean11)
kmean13<-kmeans(normdata,13)
str(kmean13)
kmean8<-kmeans(normdata,8)
str(kmean8)
#using elbow curve and trying differnt models we get that optimum no. of cluster is 13 which has minimum twss and les tot.

final2<- data.frame(airlines, kmean13$cluster)
aggregate(airlines[,2:11],by=list(kmean13$cluster), FUN=mean)
final1<-final[,c(ncol(final2),1:(ncol(final2)-1))]
View(final1)
write.csv(final1,file="airlines kmean.csv",row.names= F)
getwd()
#here total within sum of squares is minimum for 13 clusters
#so we choose it as optimum no. of clusters
#the table representing which member belongs to which cluster is created.









