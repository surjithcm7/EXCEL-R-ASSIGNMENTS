#movies association
library(arules)
library(arulesViz)
movies <- read.csv(file.choose())
movie1 <- movies[-c(1:5)]
rules <- apriori(as.matrix(movie1),parameter = list(support=0.002,confidence=0.05,minlen=3))
rules1 <- rules[is.redundant(rules)]
inspect(rules1)
rules1
#set of 29 rules 
inspect(head(sort(rules1,by="lift")))
plot(rules1)

#for support 0.007 and confidence value 0.08
rules2 <- apriori(as.matrix(movie1),parameter = list(support=0.007,confidence=0.09))

inspect(rules2)
rules2
#set of 127 rules
inspect(head(sort(rules2,by="lift")))
plot(rules2)
