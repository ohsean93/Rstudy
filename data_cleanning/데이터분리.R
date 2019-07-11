#데이터 분리
a=(split(iris,iris$Species))
head(iris3)
str(iris3)

class(iris3)

x=sample(1:10,7)
x=sort(x)

seed=split(iris,1:10)

train=seed[x]
str(train)
NROW(train)
length(train)
class(train)

train1=c()
for (i in 1:7) 
{
  train1=rbind(train1,train[[i]])
  
}
train1
dim(train1)

test=seed[-x]
str(test)
NROW(test)

test1=c()
for (i in 1:3) 
{
  test1=rbind(test1,test[[i]])
  
}
dim(test1)

train1[2,]


#ifelse연습
autoparts=read.csv("c:/rstudy/11/rtest/autoparts.csv")
head(autoparts)
autoparts1=autoparts[autoparts$prod_no=='90784-76001',c(2:11)]
autoparts2=autoparts1[autoparts$c_thickness<1000,]
autoparts3=autoparts1[autoparts$c_thickness<1000&autoparts$highpressure_time<1000,]


head(autoparts1)
head(autoparts2)

corrgram(autoparts1)
corrgram(autoparts2)
corrgram(autoparts3,upper.panel=panel.conf)

summary(autoparts3)
summary(autoparts2)

autoparts2$y_fulty=ifelse(autoparts2$c_thickness>=20&autoparts2<=32,0,1)


autoparts2$g_class=ifelse(autoparts2$c_thickness<20,1,
                          ifelse(autoparts2$c_thickness<32,2,3))

autoparts2$g_class=as.factor(autoparts2$g_class)
str(autoparts2$g_class)

plot(autoparts3[,c(5:10)])


Data=read.csv("c:/rstudy/11/rtest/Titanic/train.csv")
str(Data$Age)
mean(Data[Data$Age<15&Data$Parch>0,]$Survived,na.rm = T)

mean(Data[Data$Age<15&Data$Parch==1,]$Survived,na.rm = T)
mean(Data[Data$Age<15&Data$Parch==0,]$Survived,na.rm = T)

Data[Data$Parch>0,]$Survived
