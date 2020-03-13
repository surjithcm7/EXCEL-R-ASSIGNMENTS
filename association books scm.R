# books data 
library(arules)
library(arulesViz)

book <- read.csv(file.choose())
summary(book)
str(book)

#applying aprori algoritham for a rules.
rules <- apriori(as.matrix(book),parameter = list(support=0.002,confidence=0.05,minlen=3))
rules1 <- rules[is.redundant(rules)]
rules
#set of 8434 rules 
#inspecting rules
inspect(rules1[1:100])
#inspecting top 6 rules
inspect(head(sort(rules1,by="lift")))
head(quality(rules1))
#plotting rules
plot(rules1)
plot(rules1,method = "graph")
plot(rules1,method = "grouped")

#Trying different values of support and confidence support=0.005,confidence=0.9

rulesf <- apriori(as.matrix(book),parameter = list(support=0.005,confidence=0.09,minlen=3))
rulesf1 <- rulesf[is.redundant(rulesf)]
rulesf1
#rules no decresed to 3637 rules 
inspect(head(sort(rulesf1,by="lift")))
plot(rulesf1)

#Changing the minimum length in apriori algorithm 3 to 5
rulesl <- apriori(as.matrix(book),parameter = list(support=0.002,confidence=0.05,minlen=5))
rulesl
# 6623 rules number reduced 
