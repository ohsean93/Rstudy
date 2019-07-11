#행열병합

a=iris
b=rbind(a,c(1,2,3,"setosa"))
b=rbind(a,c(1,2,3,"setosa",1))

b=rbind(a,c(1,2,4,3,"setosa"))
tail(b)
str(b)

d1=data.frame(Sepal.Length=1,Sepal.Width=1,Petal.Length=1,Petal.Width=1,Species="setosa")
b=rbind(a,d1)
tail(b)
str(b)

iris.Sepal=cbind(iris$Sepal.Length,iris$Sepal.Width);head(iris.Sepal)
class(iris.Sepal)
iris.Sepal=data.frame(iris.Sepal)

d=data.frame(iris$Sepal.Length)
iris.Sepal=cbind(d,iris$Sepal.Width);head(iris.Sepal)


#연습문제1
d=data.frame(Sepal.Length=iris$Sepal.Length)
d$Sepal.Width=iris$Sepal.Width
d$Species=iris$Species

d

#합치기
#library(dplyr)
name=c("James","Mary","John","Lion")
age=c(72,73,74,89)
math=c(70,80,90,100)
d1=data.frame(name,age,math)

name=c("James","John","Mary")
age=c(72,74,73)
english=c(80,70,90)
d2=data.frame(name,age,english)

merge(d1,d2)
merge(d1,d2,by="name")
merge(d1,d2,all = T)
left_join(d1,d2)
left_join(d1,d2,"name")
left_join(d1,d2,c("name","age"))
left_join(d1,d2,c("name","age"),T)
cbind(d1,d2)

#연습문제2
Data=read.csv("c:/rstudy/11/rtest/Titanic/train.csv")
str(Data)
selected=cbind(Data$Pclass,Data$Sex,Data$Age,Data$SibSp,Data$Parch,Data$Fare,Data$Embarked)

head(selected)

test=read.csv("c:/rstudy/11/rtest/Titanic/test.csv")
selected_test=cbind(test$Pclass,test$Sex,test$Age,test$SibSp,test$Parch,test$Fare,test$Embarked)

all_data=rbind(selected_test,selected)
head(all_data)
colnames(all_data)=c("Pclass","Sex","Age","SibSp","Parch","Fare","Embarbed")
all_data


#행열 요약
summary(iris)
#install.packages("doBy")
#library(doBy)
summaryBy(Sepal.Length~Species,iris)

summary(all_data)
summary(Data)

#정렬

x=sample(1:5,5,F)
sort(x)

sort(iris$Sepal.Length,decreasing = T)

i=order(iris$Sepal.Length,decreasing = T)
iris$Sepal.Length[i]

iris[order(iris$Sepal.Length,iris$Petal.Length),]


#연습문제
lapply(mtcars, sort)$hp[1:5]
lapply(mtcars, sort,decreasing = T)$hp[1:5]

row.names(mtcars[lapply(mtcars, order,decreasing = T)$hp,])

mtcars[order(mtcars$hp,decreasing = T)[1:5],]$hp
mtcars[order(mtcars$hp)[1:5],]$hp

row.names(mtcars[order(mtcars$hp,decreasing = T),])

row.names(mtcars[order(mtcars$gear,mtcars$cyl,decreasing = T)[1],])
row.names(mtcars[order(mtcars$gear,mtcars$cyl,decreasing = T),][1,])


