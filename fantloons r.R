fant<- read.csv(file.choose())
View(fant)
summary(fant)
attach(fant)
?prop.test
table1<-table(Weekdays,Weekend)
table1
prop.test(x=c(66,167),n=c(233,167),conf.level = 0.95,correct = FALSE,alternative = "two.sided")

prop.test(x=c(66,167),n=c(233,167),conf.level = 0.95,correct = FALSE,alternative = "greater")
