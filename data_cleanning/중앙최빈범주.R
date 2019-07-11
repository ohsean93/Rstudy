#범위(range) 분산(var) 표준편차(sd)

w=c(72, 67, 60, 78, 82)
range(w)
max(w)-min(w)

var(w)
sd(w)

#연습문제
Data=read.csv("c:/rstudy/11/rtest/Titanic/train.csv")
head(Data)
str(Data)
tail(Data)

mean(Data$Age,na.rm=T)
var(Data$Age,na.rm=T)
sd(Data$Age,na.rm=T)

table(Data$Pclass)
which.max(table(Data$Pclass))
max(table(Data$Pclass))

class(Data$Age)
range(Data$Age, na.rm=T)[1]
range(Data$Age, na.rm=T)[2]

range(Data$Fare)
max(Data$Fare)-min(Data$Fare)

