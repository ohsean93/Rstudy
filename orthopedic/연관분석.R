#연관분석

#install.packages("arulesViz")
#library(arulesViz)

data("Groceries")
summary(Groceries)
str(Groceries)

class(Groceries)

rules<- apriori(Groceries,parameter = list(support=0.001,confidence=0.5))

rules

inspect(head(sort(rules,by="lift"),10))


inspect(Groceries[1:20])
inspect(Groceries[14])
inspect(head(Groceries[,1],20))

plot(rules)

subrules <- head(sort(rules,by="lift"),10)
plot(subrules,method = "graph",control = list(type="items"))



