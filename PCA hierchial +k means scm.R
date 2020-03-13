library(readr)

data<-read.csv(file.choose())
View(data)

#since type column is irrelevant we can remove it
mydata<-(data[-1])

View(mydata)
attach(mydata)
cor(mydata)
#the correlation values are somewhere negative somewhere positive somewhere high 
#which implies the columns are correlated each other

################## model pca  ########################
PCA<-princomp(mydata, cor = TRUE, scores = TRUE, covmat = NULL)
summary(PCA)

#here we can see that the first 3 components give us 92% information about the data

loadings(PCA)
plot(PCA)
#here we can see that the first 5 componnts are much important. 
#out of those 1st component is the most important one

biplot(PCA)
plot(cumsum(PCA$sdev*PCA$sdev)*100/(sum(PCA$sdev*PCA$sdev)),type="b")
# Showing the increase of variance with considering principal components
# Which helps in choosing number of principal components
#since we have to cluster using only 3 components we take those 3 only

pca3<-PCA$scores[,1:3]
mydata2<-cbind(mydata,pca3)
View(mydata2)
#now we have to perform clustering on both the datasets.
#that is for before performing pca and after performing the pca.

# Normalizing the data 
normdata<-scale(mydata[,1:(ncol(mydata))]) 
View(normdata)
d<-dist(normdata,method="euclidian")
fit<-hclust(d,method="complete")
plot(fit)
plot(fit,hang=-1)

#choose k=sqrt(n/2)=9
rect.hclust(fit,k=9,border="blue")
groups<-cutree(fit,k=10)
membership<-as.matrix(groups)
final<-data.frame(mydata,membership)
View(final)
final1<-final[,c(ncol(final),1:(ncol(final)-1))]

View(final1)
write.csv(final1, file="final wine hierarchial scm1.csv",row.names = F)
getwd()
aggregate(mydata,by=list(final1$membership),mean)

#now we perform clustering on the data on which we have performed PCA
normdata<-scale(mydata2[,14:(ncol(mydata2))]) 
View(normdata)
d<-dist(normdata,method="euclidian")
fit<-hclust(d,method="complete")
plot(fit)
plot(fit,hang=-1)

#choose k=sqrt(n/2)=9
rect.hclust(fit,k=9,border="red")
groups<-cutree(fit,k=10)
membership<-as.matrix(groups)
final<-data.frame(mydata,membership)
View(final)
final1<-final[,c(ncol(final),1:(ncol(final)-1))]
View(final1)
write.csv(final1, file="final wine hierarchial with pca scm.csv",row.names = F)

getwd()


aggregate(mydata,by=list(final1$membership),mean)

#from this we can see that there is change in clustering before and after performing pca 
#membership values are changed for many rows

###############performing k means clustering############################

normdata<-scale(mydata[,1:(ncol(mydata))])
wss = NULL

kmean1<- kmeans(normdata,10)
str(kmean1)
library(animation)
kmeans.ani(mydata,9)
wss = (nrow(normdata)-1)*sum(apply(normdata, 2, var))
for (i in 2:15) wss[i] = sum(kmeans(normdata, centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")   # Look for an "elbow" in the scree plot #
title(sub = "K-Means Clustering Scree-Plot")


twss <- NULL
for (i in 2:15){
  twss <- c(twss,kmeans(normdata,i)$tot.withinss)
}

twss
plot(2:15,twss,type="o")

kmean10 <- kmeans(normdata,10)
str(kmean10)
kmean11<-kmeans(normdata,11)
str(kmean11)

#using elbow curve we get that optimum no. of cluster is 11 which has minimum twss 
final2<- data.frame(mydata, kmean11$cluster)
ncol(mydata)
aggregate(mydata[,1:13],by=list(kmean11$cluster), FUN=mean)
final1<-final[,c(ncol(final2),1:(ncol(final2)-1))]
View(final1)
write.csv(final1,file="wine kmean scm.csv",row.names= F)
getwd()

# now we perform kmeans clustering on the data on whichnwe have performed pca
normdata<-scale(mydata2[,11:(ncol(mydata2))])
wss = NULL

kmean1<- kmeans(normdata,9)
str(kmean1)
wss = (nrow(normdata)-1)*sum(apply(normdata, 2, var))
for (i in 2:15) wss[i] = sum(kmeans(normdata, centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")   # Look for an "elbow" in the scree plot #
title(sub = "K-Means Clustering Scree-Plot")


twss <- NULL
for (i in 2:15){
  twss <- c(twss,kmeans(normdata,i)$tot.withinss)
}

twss
plot(2:15,twss,type="o")

kmean3 <- kmeans(normdata,3)
str(kmean3)
kmean8<-kmeans(normdata,8)
str(kmean8)
kmean13<-kmeans(normdata,13)
str(kmean13)
#using elbow curve we get that optimum no. of cluster is 13 which has minimum twss 
final2<- data.frame(mydata, kmean13$cluster)
aggregate(mydata[,1:13],by=list(kmean13$cluster), FUN=mean)
final1<-final[,c(ncol(final2),1:(ncol(final2)-1))]
View(final1)
write.csv(final1,file="wine kmean with pca.csv",row.names= F)
getwd()

#here we can see that the optimum no. of clusters using k means differs for the data set 
#before we apply PCA no. of clusters was 11 and after the application of pca no. of clusters is 13.
#also we can see that the membership changes before and after applying pca

#so by applying PCA clustering groups changes in both hierchial and k means clustering.