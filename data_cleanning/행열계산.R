#행과 열의 계산
apply(x,1,func)
lapply(list, function)
sapply(list, function)
rapply(list, function)

x=matrix(c(1:9),nrow = 3)

apply(x,2,sqrt)

func=function(x)
{
  return(x+1)
}

func2=function(x1,x2)
{
  return(x1+x2)
}

func3=function(x1,x2,x3)
{
  x1+x2+x3
}

x=matrix(c(1:9),nrow = 3)
apply(x,2,func2,1)
x1=rbind(x,apply(x,2,sum),apply(x,2,mean))

x2=cbind(x1,apply(x1,1,sum),apply(x1,1,mean))
x2

head(iris)

apply(iris[1:4], 2,sum)

lapply(x, sum)
a=lapply(iris[1:2], sum)
class(a)

class(unlist(a))
a2=matrix(unlist(a),ncol = 2)
df=data.frame(a2)
names(df)=names(iris[1:2])

df

df2=data.frame(a2)
names(df2)=names(iris[1:2])

df2

class(iris)

lapply(iris[1:4],sum)
lapply(iris[1:4],sqrt)

m=matrix(1:9,ncol = 3)
df=data.frame(m)

lapply(m, func)[[4]]
lapply(df, func)[[2]][1]

lapply(m, func2,3)[[2]]
lapply(df, func2,3)[[1]][2]

lapply(m, func3,3,1)[[1]]
lapply(df, func3,3,1)[[1]][1]

x
df
sapply(x,sum)
sapply(x,sqrt)
class(sapply(df,sum))
class(sapply(df,sqrt))
sapply(iris[1:4],sum)
sapply(iris[1:4],sqrt)
t(data.frame(sapply(iris[,1:4],mean)))

#연습문제2
dim(mtcars)
head(mtcars)

apply(mtcars,2,mean)

str(mtcars)

apply(mtcars[8:14,], 2,mean)

tapply(iris$Sepal.Length,iris$Species,sum)


#연습문제3
colSums(mtcars)
colSums(mtcars[8:14,])

colMeans(mtcars)
colMeans(mtcars[8:14,])

#연습문제4
lapply(mtcars,sum)#list형태
unlist(lapply(mtcars,sum))

sapply(mtcars, sum)

#연습문제5
mtcars
vs_mtcars=tapply(mtcars$hp, mtcars$vs, mean)
abs(vs_mtcars[1]-vs_mtcars[2])

mtcars_hp_for_gear=tapply(mtcars$hp, mtcars$gear, mean)
mtcars_hp_for_gear

#연습문제6
txt=readLines("C:/rstudy/11/rtest/sample.txt")
class(txt)#char형 벡터
noun=lapply(txt,getMorph, "noun")
#"noun"에 들어가는 형태소를 기반으로 각 리스트 별로 어떤 작업을 시행하는
#함수 getMorph가 정의되어야 한다.










######
library(readxl)
dil01=read_excel("C:/rstudy/11/khu_edu/example_set/2015_01Data/Delivery1501.xlsx")
ord01=read_excel("C:/rstudy/11/khu_edu/example_set/2015_01Data/Order1501.xlsx")
head(ord01)
a=tapply(ord01$MarketPriceAmt, ord01$OrderDate, sum)
barplot(a)
ord01_01=ord01[,c("OrderDate","MarketPriceAmt","OrderQty")]
ord01_01$OrderDate=format.Date(ord01_01$OrderDate,"%a")
ord01_01$OrderDate=as.factor(ord01_01$OrderDate)
b=tapply(ord01$MarketPriceAmt, ord01$OrderDate, sum)
barplot(b)

