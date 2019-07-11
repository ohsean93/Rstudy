#C:\rstudy\11\rtest

Data=read.csv("c:/rstudy/11/rtest/Titanic/train.csv")

#sample replce=T/F(중복 허가/비허가), prob=c(가중치)

a=sample(c('a','b','c','d','e'),160,T,c(6,1,5,1,3))
b=sample(c('a','b','c','d','e'),160,T)

table(a)
table(b)

#층화 임의 추출
#install.packages("sampling")
#library(sampling)

m1=strata(c("Species"),size = c(2,3,4),method = "srswor",data=iris)

table(Data$Pclass)
strata(c("Pclass"),size = c(5,2,2),method = "srswor",data=Data)


m2=getdata(iris,m1)
class(m1);class(m2)
dim(m1);dim(m2)
m1;m2


#연습문제
item=c('a','b','c','d','e')
sales=c(30,5,25,5,15)

df=data.frame(item,sales)
sample(item,10,T,sales)

total=sum(sales)
persales=sales/total
df=cbind(df,persales)

sample(item,10,T,persales)

#연습문제2
datarow=NROW(Data)[1]
rate_sur=c()

s1=round(datarow*0.05)
sr1=sample(1:datarow,s1)
Data1=Data[sr1,]
rate_sur[1]=mean(Data1$Survived)


s2=round(datarow*0.1)
sr2=sample(1:datarow,s2)
Data2=Data[sr2,]
rate_sur[2]=mean(Data2$Survived)

s3=round(datarow*0.5)
sr3=sample(1:datarow,s3)
Data3=Data[sr3,]
rate_sur[3]=mean(Data3$Survived)

rate_sur[4]=mean(Data$Survived)

rate_sur

#추가 연습문제
rate_sur_Sex=c()

rate_sur_Sex[1]=mean(Data$Survived)
rate_sur_Sex[2]=mean(Data[Data$Sex=="male",]$Survived)
rate_sur_Sex[3]=mean(Data[Data$Sex=="female",]$Survived)

tm=strata(c("Sex"),size = c(50,50),method = "srswor",data=Data)
tm2=getdata(Data,tm)

rate_sur_Sex_sample=c()
rate_sur_Sex_sample[1]=mean(tm2$Survived)
rate_sur_Sex_sample[2]=mean(tm2[tm2$Sex=='male',]$Survived)
rate_sur_Sex_sample[3]=mean(tm2[tm2$Sex=='female',]$Survived)

rate_of_sur_Sex=data.frame("Sex"=c("total","Male","Female"),"total"=rate_sur_Sex,"sample"=rate_sur_Sex_sample)
rate_of_sur_Sex


#추출과 정확도
rate_sur_5per=c()
for (i in 1:10) 
{
  s1=round(datarow*0.05)
  sr1=sample(1:datarow,s1)
  Data1=Data[sr1,]
  rate_sur_5per[i]=mean(Data1$Survived) 
}

rate_sur_10per=c()
for (i in 1:10) 
{
  s1=round(datarow*0.1)
  sr1=sample(1:datarow,s1)
  Data1=Data[sr1,]
  rate_sur_10per[i]=mean(Data1$Survived) 
}

rate_sur_50per=c()
for (i in 1:10) 
{
  s1=round(datarow*0.5)
  sr1=sample(1:datarow,s1)
  Data1=Data[sr1,]
  rate_sur_50per[i]=mean(Data1$Survived) 
}

sd(rate_sur_5per)
mean(rate_sur_5per)

sd(rate_sur_10per)
mean(rate_sur_10per)

sd(rate_sur_50per)
mean(rate_sur_50per)



