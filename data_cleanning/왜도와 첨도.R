#install.packages("moments")
#library(moments)

Data=read.csv("c:/rstudy/11/rtest/Titanic/train.csv")
Age=Data$Age
Age2=Age[is.na(Age)==0]


skewness(Age2)
kurtosis(Age2)
stem(Age2)

hist(Age2)
plot(density(Age2))

den.norm=function(x)dnorm(x,mean=mean(Age2),sd=sd(Age2))
curve(den.norm,col='red',add=T,lty=2)
abline(v=mean(Age2),col='blue',lty=2)


Fare=Data$Fare

skewness(Fare)
kurtosis(Fare)
stem(Fare)

hist(Age2)
plot(density(Fare))

den.norm=function(x)dnorm(x,mean=mean(Fare),sd=sd(Fare))
curve(den.norm,col='red',add=T,lty=2)
abline(v=mean(Fare),col='blue',lty=2)

