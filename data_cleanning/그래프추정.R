#사분위수
x=c(136,182,166,132,130,186,140,155)
x
sort(x)
quantile(x)
summary(x)
quantile(x,c(0.25,0.3))

summary(Data)
summary(iris)
str(iris)

boxplot(x)


y=sample(80:100,20)

boxplot(y)


z=sample(40:166,18)
z=c(39, 128,  57, 147, 149, 116, 102,  66,  41, 103, 164,  80, 124,  88, 150, 134, 138,  42, 104,167)
z=z/10
z
znum=length(z)
p=round(znum^(1/3))
round((max(z)-min(z))/p)
z.cut=cut(z,breaks = seq(3,19,by=4),right=F)
z.cut
table(z.cut)

install.packages("devtools")
library(devtools)
install_github("SukjaeChoi/easyStats")
library(easyStats)
z1=freqTable(z)
z1
#round옵션 digits=소수점 아래 자리수
round(prop.table(z1),digits=3)*100

#연습문제
quantile(Data$Fare)

quantile(Data$Age,na.rm=T)
Age=Data$Age
Fare=Data$Fare

a=cbind(Age=Age,Fare=Fare)
summary(a)

boxplot(Age)
boxplot(Fare)

Data=read.csv("c:/rstudy/11/rtest/Titanic/train.csv")
Age2=Age[is.na(Age)==0]

Age3=freqTable(Age2)

Age3

plot(Age2)
boxplot(Age2)
hist(Age2)
barplot(Age2)
plot(Age3)
barplot(Age3)
hist(Age3)
stem(Age2)


exam1=sample(10:50,20)
stem(exam1)
exam2=sample(10:50,20,T)
stem(exam2)

hist(Data$Age)
hist(Data$Pclass)
hist(Data$Fare)
summary(Data)
plot(Data$Age)
