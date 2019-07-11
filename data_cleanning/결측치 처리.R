#결측치 처리
x=c(1,NA,2,3,4)
is.na(x)
complete.cases(x)

autoparts=read.csv("c:/rstudy/11/rtest/autoparts.csv")
autoparts1=autoparts[autoparts$prod_no=='90784-76001',c(2:11)]
autoparts2=autoparts1[autoparts$c_thickness<1000,]
autoparts3=autoparts1[autoparts$c_thickness<1000&autoparts$highpressure_time<1000,]

dim(autoparts)
sum(is.na(autoparts))

#install.packages("reshape2")
#library(reshape2)

dim(french_fries)
head(french_fries)

french_fries[!complete.cases(french_fries),]
french_fries[is.na(french_fries),]
sum(!complete.cases(french_fries))
sum(is.na(french_fries))
is.na(french_fries)==(!complete.cases(french_fries))
class(is.na(french_fries))
class(!complete.cases(french_fries))

corrgram(french_fries)

na.idx=which(!complete.cases(french_fries))
df.new=french_fries[-na.idx]

french_fries


x
y=1:5
df=data.frame(x,y)

na.omit(df)
na.fail(df)
na.pass(df)

resid(lm(x~y,data=df,na.action = na.omit))
resid(lm(x~y,data=df,na.action = na.exclude))


x[is.na(x)]=0
x          

french_fries[is.na(french_fries)]=0
french_fries[na.idx,]
french_fries[!complete.cases(french_fries),]


#연습문제
sample1=sample(c(1,3,4,5,NA),20,T)
sample2=sample(c(6,7,8,NA),20,T)
sample3=sample(c(9,10,NA),20,T)

sam=data.frame(sample1,sample2,sample3)

sam
sam.exit=sam
sam.sub=sam

sam.exit=sam.exit[-which(!complete.cases(sam)),]
sam.sub[,1]

for (i in 1:3) 
{
  sam.sub[which(!complete.cases(sam[,i])),i]=mean(sam[,i],na.rm = T)
}

sam.exit
sam.sub

summary(sam)
summary(sam.exit)
summary(sam.sub)

#연습문제2
Data=read.csv("c:/rstudy/11/rtest/Titanic/train.csv")

error1=which(!complete.cases(Data))
error2=which(!complete.cases(Data)|Data$Cabin==""|Data$Embarked=="")
error1
error2
Data$Age2=Data$Age
Data$Age2[is.na(Data$Age)]=median(Data$Age,na.rm = T)
sum(is.na(Data$Age2))
