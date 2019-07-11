#부스팅

boo.adabag<-boosting(Species~.,data=iris,boos = T,mfinal=10)

boo.adabag$importance

plot(boo.adabag$trees[[10]])
text(boo.adabag$trees[[10]])

pred<-predict(boo.adabag,newdata=iris)

table1<-table(pred$class,iris[,5])

table1

(table1[1,1]+table1[2,2]+table1[3,3])/sum(table1)

1-(sum(diag(table1))/sum(table1))

