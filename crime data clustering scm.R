library(readr)
crime <- read.csv(file.choose())
View(crime)
#checking normality
summary(crime)
attach(crime)
qqnorm(Murder)
qqline(Murder)
qqnorm(Assault)
qqline(Assault)
qqnorm(UrbanPop)
qqline(UrbanPop)
qqnorm(Rape)
qqline(Rape)

#most data follows normal distribution. 
#boxplot
boxplot(Murder)
boxplot(Assault)
boxplot(UrbanPop)
boxplot(Rape)
#there are some outliers in data and all data are in differnt scale so we have to normalize this data.
normdata<-scale(crime[2:5])
normdata

d<-dist(normdata,method="euclidian")
fitvalue<-hclust(d,method="complete")
plot(fitvalue,hang=-1)


#we make use of the formula k=sqrt(n/2=)=5 for finding the no. of clusters
clu<-rect.hclust(fitvalue,k=5,border="blue")
cluster<-cutree(fitvalue,k=5)
cluno<-as.matrix(cluster)
final<-data.frame(crime,cluno)
View(final)

#merging to first colum
final1<-final[,c(ncol(final),1:(ncol(final)-1))]
View(final1)
#changing to excel and saving
write.csv(final1,file="crime data final.csv",row.names= F)
getwd()







