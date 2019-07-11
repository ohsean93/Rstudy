#library(doBy)

강원<-c(200,500,400)
경남<-c(400,300,500)
충북<-c(600,300,400)

sales<-data.frame(강원,경남,충북)
sales<-stack(sales)

sales2<-data.frame(branch=sales$ind,sales=sales$values)

colnames(sales)<-c('sales','branch')

summaryBy(sales~branch,sales2)

sales3<-unstack(sales2)
sales4<-unstack(sales)
sales5<-unstack(sales2,sales~branch)

sales
sales2
sales3
sales4
sales5

class(sales3)
class(sales5)

library(reshape2)
sale<-data.frame(강원,경남,충북)
sale<-melt(sale)
sale

sale2<-data.frame(branch=sale$variable,sale=sale$value)

colnames(sale)<-c('branch','sale')

summaryBy(sale~branch,sale2)

sale5<-dcast(sale2,sale~branch)

class(sale5)

df2<-french_fries
head(df2)
str(df2)
df22<-(melt(french_fries,id.vars = 1:4))
head(df22)

#연습문제1
apple=c(6,10,13)
banana=c(2,8,10)
peach=c(7,3,5)
berry=c(9,15,11)
year=c(2000,2001,2002)

fruit=data.frame(apple,banana,peach,berry,year)

summaryBy(values~ind,stack(fruit[1:4]))
summaryBy(value~variable,melt(fruit[1:4]))
melt(fruit,id.vars = 5)
a=dcast(melt(fruit,id.vars = 5),year~variable)
a
