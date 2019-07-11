#install.packages("dplyr")
#library(dplyr)

test=read.csv("c:/rstudy/11/rtest/test.csv")
test=`names<-`(test,c("item","price","sales"))

x=c(1:5)
y=seq(10,50,10)
z=c("M","M","M","F","F")

d=data.frame(x,y,z)
d
str(d)

id=c(1:5)
factory=c("평택지부1","평택지부2","안산지부","인천지부","원주지부")
sales=c(70,90,80,85,87)

d=data.frame(id,factory,sales,stringsAsFactors = T)
str(d)

id=c(1:5)
factory=c("평택1","평택2","안산","인천","원주")
taype=c("공장","공장","오피스","오피스","오피스")
sales=c(70,90,80,85,87)

d=data.frame(id,factory,taype,sales,stringsAsFactors = F)
d$taype=as.factor(d$taype)
str(d)

id=c(1:7)
name=c('김원경','박찬웅','조해선','김선영','이화영','양영욱','최필선')
gender=c('F','M','F','F','F','M','M')
sales=c(1000,2000,1500,2200,1700,2000,2200)

d=data.frame(id,name,gender,sales)

d$name==d[,2]
d[,2]==d[,"name"]



d%>%select(name)

d[,c(2:4)]==d%>%select(name,gender,sales)

d[d$gender=="F"&sales>2000,]
str(d)


  m.man=subset(d,gender="M")
class(m.man)  

d.man=d[which(gender=="M"),]
  d.man

d.man=d%>%filter(gender=="M")%>%select(name)
d.man

d%>%filter(gender=="F")
d%>%filter(gender=="F"&sales>2000)
d%>%filter(gender=="F"|sales>2000)

#연습문제
test

test[test$sales>=5,]
test[test$price>300&test$sales>=5,]
test[test$price>300&test$sales>=5,c(1)]

subset(test,sales>=5)
subset(test,price>300&sales>=5)
subset(test,price>300&sales>=5,select = item)

#행,열 제거
d[-1,-2]

x1=1:3
x2=4:6
x3=7:9
x4=10:12
x5=c("James","Mary","Tony")
d=data.frame(x1,x2,x3)

d2=cbind(d,x4)
d3=cbind(d2,x5,stringsAsFactors=F)

d=data.frame(x1,x2,x3)
d$x4=x4
d
d$x5=x5
d

d=data.frame(x1,x2,x3)
d$sum=d$x1+d$x2+d$x3
d
d$pass=ifelse(d$sum>15,"pass","fail")
d

id=1:5
mid=c(30,40,50,60,70)
a=data.frame(id,mid)

id=5:1
final=c(70,90,100,90,80)
b=data.frame(id,final)

left_join(a,b,by="id")
merge(a,b)

d
d=rename(d,y1=x1)
d

#연습문제
str(Data$Sex)
Data=read.csv("c:/rstudy/11/rtest/Titanic/train.csv")
train_male=Data%>%filter(Sex=="male")
train_female=Data%>%filter(Sex=="female")

mean_sur_male=mean(train_male$Survived,na.rm = T)
mean_sur_female=mean(train_female$Survived,na.rm = T)

mean_sur=data.frame("gender"=c("male","female"),"rate_sur"=c(mean_sur_male,mean_sur_female))
mean_sur

str(Data)
sur_male_age00=mean(Data$Survived[Data$Sex=="male"&Data$Age<10],na.rm = T)
sur_male_age10=mean(Data$Survived[Data$Sex=="male"&Data$Age>=10&Data$Age<20],na.rm = T)
sur_male_age20=mean(Data$Survived[Data$Sex=="male"&Data$Age>=20&Data$Age<30],na.rm = T)

sur_female_age00=mean(Data$Survived[Data$Sex=="female"&Data$Age<10],na.rm = T)
sur_female_age10=mean(Data$Survived[Data$Sex=="female"&Data$Age>=10&Data$Age<20],na.rm = T)
sur_female_age20=mean(Data$Survived[Data$Sex=="female"&Data$Age>=20&Data$Age<30],na.rm = T)

sur_gen_age=data.frame(gender=c("male","male","male","female","female","female"),
                       age=c(0,10,20,0,10,20),
                       rate_sur=c(sur_male_age00,sur_male_age10,sur_male_age20,
                                  sur_female_age00,sur_female_age10,sur_female_age20))

sur_gen_age

