#단순회귀와 SVM회귀 분석

regression<-read.csv("C:/rstudy/11/rtest/regression.CSV")


lmregression<-lm(y~x,data = regression)
p1=predict(lmregression,newdata = regression)

svmregression<-svm(y~x,data=regression)
p2=predict(svmregression,newdata = regression)

plot(regression$x,regression$y)
points(regression$x,p1,col="blue",pch="L")
points(regression$x,p2,col="red",pch="s")
