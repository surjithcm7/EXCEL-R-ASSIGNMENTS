library(readr)
cot<-read.csv(file.choose())
View(cot)
summary(cot)
cot1<-ifelse(cot=="Error Free",0,1)
View(cot1)
table(cot1)
chisq.test(table(cot1))
