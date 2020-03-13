#paired T test for cutlet data
cutlet<- read.csv(file.choose())
View(cutlet)
attach(cutlet)

y1 <- Unit.A
y2 <- Unit.B
# checking normality
#Normality test
#Ho : data are normal
#ha: data are not normal
shapiro.test(y1)# p-0.5>0.05=> data are normal
shapiro.test(y2)# p- 0.16>0.05=> data are normal
# external conditions are same- Paired T test

#Case1:

#HO: mu of y1= mu of y2
#Ha: mu of y1!= mu of y2

t.test(y1,y2,alternative = "two.sided",conf.level = 0.95,paired = TRUE)
