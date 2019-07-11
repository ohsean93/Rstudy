#배깅

#단순 임의 복원추출

#install.packages("adabag")
#library(adabag)
#library(caret)
#install.packages("stringi")

iris.baging<-bagging(Species~.,data=iris,mfinal=10)

iris.baging$importance

plot(iris.baging$trees[[1]])
text(iris.baging$trees[[1]])
class(iris.baging$trees)


pred<-predict(iris.baging,newdata=iris)

table1<-table(pred$class,iris[,5])

(table1[1,1]+table1[2,2]+table1[3,3])/sum(table1)


