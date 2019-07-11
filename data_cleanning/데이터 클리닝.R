#feature engineering
pampas=c(283,288,205,204,287,300,310)
milk=c(33,31,31,32,33,34,29)
tissue=c(2500,2450,2490,2750,2800,2350,2450)

#고수준 작도함수 생성
plot(NULL,NULL,xlim=c(0,10),ylim = c(0,3000))

#저수준 작도함수
lines(pampas,type = "b",col="blue")
lines(milk,type = "b",col="red")
lines(tissue,type = "b",col="black")

#스케일 실시
scale(pampas)
scale(milk)
scale(tissue)

plot(NULL,NULL,xlim=c(0,10),ylim = c(-3,3))
lines(scale(pampas),type = "b",col="blue")
lines(scale(milk),type = "b",col="red")
lines(scale(tissue),type = "b",col="black")

#실습
sc=scale(iris[1:4]);head(sc)
df=as.data.frame(sc);head(df)
cb=cbind(df,"Species"=iris$Species);head(cb)

plot(cb)
plot(iris)

#binning
agemoney=read.csv("c:/rstudy/11/rtest/agemoney.csv")
agemoney
age10=mean(agemoney[agemoney$age>=10&agemoney$age<20,2])
age20=mean(agemoney[agemoney$age>=20&agemoney$age<30,2])
age30=mean(agemoney[agemoney$age>=30&agemoney$age<40,2])
age40=mean(agemoney[agemoney$age>=40&agemoney$age<50,2])
age50=mean(agemoney[agemoney$age>=50&agemoney$age<=60,2])
data.mean=data.frame(age=c(10,20,30,40,50),money=c(age10,age20,age30,age40,age50))
data.mean
plot(data.mean$age,data.mean$money)
plot(agemoney$age,agemoney$money)

tail(agemoney,10)

#creating feature
data.txt=c("2016-11-01","2016-11-03","2016-11-05")
class(data.txt)

date=as.Date(data.txt)
class(date)

format(date,format="%Y-%m-%A")
format(date,format="%A")

#creating dummy
lvl=factor(c("A","B","A","A","C"))
lvl
df=data.frame(lvl);df

dv=model.matrix(~lvl,data = df);dv
dv=model.matrix(~lvl,data = df)[,-1];dv


