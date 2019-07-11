#상관분석0.3~0.5=>상관관계있음, 0.7~강한 상관관계

cor(iris$Sepal.Width,iris$Sepal.Length)
plot(iris$Sepal.Width,iris$Sepal.Length)

cor(iris$Petal.Width,iris$Petal.Length)
plot(iris$Petal.Width,iris$Petal.Length)

x1=c(1,5,8,4,3)
x2=c(8,15,20,13,5)
cor(x1,x2)
plot(x1,x2)

x=iris[,1:4]
plot(x)
cor(x)
symnum(cor(x))

#install.packages("corrgram")
#library(corrgram)

corrgram(x)
corrgram(iris,upper.panel=panel.conf )

str(Data)
cor(Data$Pclass,Data$Fare)


a=Data[,c(2,3,5,10)]
head(a)
a$Sex=as.integer(a$Sex)
cor(a)
symnum(cor(a))
corrgram(a)
corrgram(a,upper.panel=panel.conf )

cor(a,method = "spearman")
symnum(cor(a,method = "spearman"))
