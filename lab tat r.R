#annova hospital data 
lab<- read.csv(file.choose())
View(lab)
Stacked_Data <- stack(lab)
View(Stacked_Data)
attach(Stacked_Data)
# data normality check
shapiro.test(lab$Laboratory.1)
shapiro.test(lab$Laboratory.2)
shapiro.test(lab$Laboratory.3)
shapiro.test(lab$Laboratory.4)
shapiro.test(Stacked_Data)
summary(lab)
library(car)
#checking variance equality
leveneTest(values~ ind, data = Stacked_Data)
Anova_results <- aov(values~ind,data = Stacked_Data)
summary(Anova_results)
