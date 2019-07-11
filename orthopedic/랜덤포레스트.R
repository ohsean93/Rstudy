#랜덤포레스트

#단순 임의 복원추출->부스트렙

#install.packages("randomForest")
#library(randomForest)

data("stagec")
stage3<-stagec[complete.cases(stagec),]
set.seed(1:4)
ind<-sample(2,nrow(stage3),replace = T,prob = c(0.7,0.3))
traindata<-stage3[ind==1,]
testdata<-stage3[ind==2,]

  
rf<-randomForest(ploidy~.,data=traindata,ntree=100,proximity=T)

table(predict(rf),traindata$ploidy)

plot(rf)

rf$importance

table(predict(rf,newdata = testdata),testdata$ploidy)

plot(margin(rf))
