#데이터테이블
library(data.table)
update.packages()

df<-data.frame(name=c("james","mary","kevin"),age=c(22,23,24))
df
dt<-data.table(name=c("james","mary","kevin"),age=c(22,23,24))
dt

class(dt)

str(df)

str(dt)

iris.dt<-as.data.table(iris)
iris.dt[c(10:30,100)]

Data<-read.csv("c:/rstudy/11/rtest/Titanic/train.csv")
Data.dt<-fread("c:/rstudy/11/rtest/Titanic/train.csv")

str(Data)
str(Data.dt)

iris.dt[,1]
iris.dt[1,]==iris.dt[1]

iris.dt$Sepal.Length
iris.dt$"Sepal.Length"
iris.dt[,Sepal.Length]
iris.dt[,"Sepal.Length"]

iris.dt[1:5,Sepal.Length]
iris.dt[1:5,"Sepal.Length"]
iris.dt[1:5,c(Sepal.Length,Species)]
iris.dt[1:5,c("Sepal.Length","Species")]

iris.dt[iris.dt$Petal.Width<1.5]
iris.dt[Petal.Width<1.5]

aggregate(Sepal.Width~Species,iris.dt,mean)
iris.dt[,mean(Sepal.Width),by=Species]
iris.dt[,mean(Sepal.Width),by=Species]
iris.dt[,list(a1=mean(Sepal.Width)),by=Species]


pima=fread("C:/rstudy/11/rtest/Pima_Data/diabetes.csv")
pima[100]

pima[,mean(Insulin),by=Outcome]











