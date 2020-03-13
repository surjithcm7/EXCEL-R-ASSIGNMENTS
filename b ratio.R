bratio<- read.csv(file.choose())
#boxplot
?boxplot
boxplot(bratio$East)
boxplot(bratio$West)
boxplot(bratio$North)
boxplot(bratio$South)
# creating table
table<-matrix(c(50,435,142,1523,131,1356,70,	750),nrow=2)
colnames(table)<-c("east","west","north","south")                 
rownames(table)<-c("males","females")
table
chisq.test(table)
