#library(corrgram)

#이상현상 탐지
autoparts=read.csv("c:/rstudy/11/rtest/autoparts.csv")
autoparts1=autoparts[autoparts$prod_no=='45231-3B610',c(2:11)]
boxplot(autoparts1$c_thickness)

#검출1, 사분위수

cleandata_q=autoparts1
a1=which(cleandata_q$c_thickness<
        (fivenum(autoparts1$c_thickness)[2]-1.5*IQR(autoparts1$c_thickness)))
cleandata_q=autoparts1[-a1,]

a2=which(cleandata_q$c_thickness>
        (fivenum(autoparts1$c_thickness)[4]+1.5*IQR(autoparts1$c_thickness)))

a=c(a1,a2)

a

cleandata_q=cleandata_q[-a2,]
boxplot(cleandata_q$c_thickness)

c1=autoparts1$c_thickness
c2=sort(c1)
a2=c2[which((c2>fivenum(c2)[4]+1.5*IQR(c2))|(c2<(fivenum(c2)[2]-1.5*IQR(c2))))]
myboxplot=boxplot(c2)
sum(myboxplot$out!=a2)

#잔차분석
m=lm(c_thickness~.,data=autoparts1)

plot(rstudent(m))

#install.packages("car")
#library(car)

x=outlierTest(m)
x$rstudent
names(x$rstudent)

summary(outlierTest(m))

#쿡의 거리
plot(m)

cooks=cooks.distance(m)
cooks
plot(cooks,pch=".",cex=1.5)

text(x=1:length(cooks),y=cooks,labels = 
       ifelse(cooks>4/nrow(autoparts1),names(cooks),""),col="red")
plot(cooks,pch=".",cex=1.5)

text(x=1:length(cooks),y=cooks,labels = 
       ifelse(cooks>1,names(cooks),""),col="red")

influential=names(cooks)[(cooks>4/nrow(autoparts1))]

autoparts1[rownames(autoparts1)%in%influential,]

#반응변수
#install.packages("outliers")
#library(outliers)

outlier(autoparts1$c_thickness)
outlier(autoparts1$c_thickness,opposite = T)

#밀도기반
#install.packages("DMwR")
#library(DMwR)

score=lofactor(autoparts1,k=5)
plot(score)
top5=order(score,decreasing = T)[1:5]
top5


#연습문제
Data=read.csv("c:/rstudy/11/rtest/Titanic/train.csv")
str(Data)
#Age, Fare
boxplot(Data$Age)
boxplot(Data$Fare)

exam1=Data$Age
exam2=Data$Fare

a1=which((exam1>fivenum(exam1)[4]+1.5*IQR(exam1,na.rm=T))|(exam1<(fivenum(exam1)[2]-1.5*IQR(exam1,na.rm=T))))
a2=which((exam2>fivenum(exam2)[4]+1.5*IQR(exam2))|(exam2<(fivenum(exam2)[2]-1.5*IQR(exam2))))
a1
a2

a=c(a1,a2)

table(a)
error1=which(!complete.cases(Data))

NROW(Data)
sum(is.na(Data$Age))

error=c(error1,a1)
table(error)

Data.exit1=Data[-a1,]
Data.exit2=Data[-a2,]
Data.exit=Data[-a,]

Data.sub1=Data
Data.sub1[a1,"Age"]=mean(Data$Age,na.rm = T)
Data.sub=Data.sub1
Data.sub[a2,"Fare"]=mean(Data$Fare,na.rm = T)

Data.sub
