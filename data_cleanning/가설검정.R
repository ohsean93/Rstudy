#install.packages("MASS")
#library(MASS)
data("survey")
head(survey)
str(survey)

xt=xtabs(~Sex+Exer,data=survey)
xt

chisq.test(xt)

xt=xtabs(~W.Hnd+Clap,data=survey)
xt

chisq.test(xt)
fisher.test(xt)

performance= matrix(c(794,86,150,570),nrow = 2,
                    dimnames = list("1st Survey"=c('Approve','Disapprove')
                                    ,"2nd Survey"=c('Approve','Disapprove')))
performance

mcnemar.test(performance)

xt=table(survey$W.Hnd)
xt
chisq.test(xt,p=c(0.3,0.7))

x=rnorm(1000)
shapiro.test(x)

ks.test(rnorm(100),rnorm(90))

#연습문제
Data=read.csv("c:/rstudy/11/rtest/Titanic/train.csv")

Data$Survived=as.factor(Data$Survived)
Data$Pclass=as.factor(Data$Pclass)
head(Data)
str(Data)

chisq.test(xtabs(~Sex+Survived,data=Data))
chisq.test(xtabs(~Pclass+Survived,data=Data))
chisq.test(xtabs(~Name+Survived,data=Data))
fisher.test(xtabs(~Name+Survived,data=Data))
str(xtabs(~Name+Survived,data=Data))

#연습문제2
exam1=matrix(c(2,5,28,25),nrow = 2,
             dimnames = list( "Cancel"=c("Y","N"),"Group"=c("A","B") ))
exam1

fisher.test(exam1)


group=c('A',"A",'B',"B")
cancer=c('1.Y','2.N','1.Y','2.N')
count=c(2,28,5,25)
dat=data.frame(group,cancer,count)
tab=xtabs(count~group+cancer,data=dat)
tab

fisher.test(exam1)
fisher.test(tab)


#연습문제 3

exam2=matrix(c(59,16,6,80),nrow = 2,
             dimnames = list( "After"=c("presnt","absent"),
                              "Before"=c("presnt","absent") ))
exam2

chisq.test(exam2)
fisher.test(exam2)

#연습문제 4
Data=read.csv("c:/rstudy/11/rtest/Titanic/train.csv")

str(Data)
#Age,Fare, pclass


shapiro.test(Data$Age)
shapiro.test(Data$Fare)
shapiro.test(Data$Pclass)
