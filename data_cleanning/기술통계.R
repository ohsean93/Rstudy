#평균(산술평균)
w=c(72, 67, 60, 78, 82)
mean(w)

#랜덤추출 replace = T/F 복원추출 허용/비허용
sample(x=1:100,size = 5, replace = T)

mynum=sample(x=1:100,size = 100, replace = T)
mynum
mean(mynum)
summary(mynum)

mynum2=mynum
n= sample(x=1:100,size = 20, replace = F)
mynum2[n]=NA
mynum2
mean(mynum2)
n
summary(mynum)
summary(mynum2)

sum(mynum2,na.rm = T)
mean(mynum2,na.rm = T)
var(mynum2,na.rm = T)
sd(mynum2,na.rm = T)

#가중평균
sales=c(95, 72, 87, 65)
weights=c(0.5,0.25,0.125,0.125)
mean(sales)
weighted.mean(sales, weights)

a.score=c(4.0,3.0)
b.score=c(3.0,4.0)
scoreweight=c(3,2)
mean.score=c(weighted.mean(a.score,scoreweight),weighted.mean(b.score,scoreweight))
mean.score


score=c(90,80,70,60)
count=c(3,12,15,5)
final.mean.score=c(weighted.mean(score,count))
final.mean.score

#중앙값 최빈값 빈도표
time=c(7,2,3,7,6,9,10,8,9,9,10)
sort(time)    #내림차순
mean(time)    #평균
median(time)  #중앙값


time2=c(7,2,3,7,6,9,10,8,9,10)
mean(time2)
median(time2)

num.v=sample(1:10,size = 30,replace=T)
num.v

char.v=sample(c('o','it','if','i'),size = 10,replace=T)
char.v

freq=table(num.v)       #빈도표
freq
which.max(freq)         #최빈값
names(which.max(freq))  #최빈값


freq2=table(char.v)
freq2
which.max(freq2)
names(which.max(freq2))

#색인구하기
a=c(1,2,NA,3)
which.max(a)
max(a)
max(a,na.rm = T)
which.min(a)
min(a)
min(a,na.rm = T)


#연습문제
dice=c(1:6)
m1=mean(dice)
sd1=sd(dice)

dice2=sample(dice,size = 10,replace=T)
dice2
m2=mean(dice2)
sd2=sd(dice2)

dice3=sample(dice,size = 1000000,replace=T)
dice3
m3=mean(dice3)
sd3=sd(dice3)

mean.t=cbind(m1,m2,m3)
sd.t=cbind(sd1,sd2,sd3)

mean.t
sd.t

#연습문제2
Data=read.csv("./rtest/Titanic/train.csv")
head(Data)
str(Data)
tail(Data)

rate_of_survied=mean(Data$Survived)
mean_of_train_age=mean(Data$Age,na.rm=T)
sd_of_train_age=sd(Data$Age,na.rm=T)
var_of_train_age=var(Data$Age,na.rm=T)

Data.survived=Data[Data$Survived==1,]
mosa=mean(Data.survived$Age, na.rm=T)

mosa2=weighted.mean(Data$Age,Data$Survived,na.rm = T)

mosa==mosa2

rate_of_survied
mean_of_train_age
sd_of_train_age
var_of_train_age

mosa
mosa2

NROW(Data)
NROW(Data.survived)

names(Data)
table(Data[c(2,5)])

head(Data[,7])
