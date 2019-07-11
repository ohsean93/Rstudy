#library(Epi) roc커브 auc 페키지
#install.packages("class")
#library(class) knn사용 페키지
#library(e1071) 최적값 찾기, knn

#데이터 가져오기
autoparts<-read.csv("C:/rstudy/11/rtest/autoparts.csv",header = T)
autoparts1<-autoparts[autoparts$prod_no=="90784-76001",c(2:11)]
autoparts2<-autoparts1[autoparts1$c_thickness<1000,]

#grouping하기
autoparts2$y_faulty <- ifelse(((autoparts2$c_thickness<20)|
                                 (autoparts2$c_thickness>32)),1,0)

head(autoparts2)

#훈련, 테스트 셋 나누기
t_index=sample(1:NROW(autoparts2),size = NROW(autoparts2)*0.7)
t_index=sort(t_index)

train<-autoparts2[t_index,]
test<-autoparts2[-t_index,]

nrow(train);nrow(test)
nrow(train)+nrow(test)==nrow(autoparts2) #확인하니 잘 나뉘었다.

head(train);head(test)

#k-nn사용
xmat_train <- as.matrix(train[1:9])
y_faulty.train <- train$y_faulty

xmat_test <- as.matrix(test[1:9])
head(xmat_test)

yhat_test<-knn(xmat_train,xmat_test,as.factor(y_faulty.train),k=3)#기본은 k=1
table1=table(real=test$y_faulty,predict=yhat_test)
table1
(table1[1,1]+table1[2,2])/sum(table1)

sum(test$y_faulty==yhat_test)/sum(table1)

#최적의 k 찾기
tune.out <- tune.knn(x=xmat_train,y=as.factor(y_faulty.train),k=1:10)
plot(tune.out)

yhat_test2<-knn(xmat_train,xmat_test,as.factor(y_faulty.train),k=5)

table2=table(real=test$y_faulty,predict=yhat_test2)
table2
(table2[1,1]+table2[2,2])/sum(table2)

sum(test$y_faulty==yhat_test2)/sum(table2)

ROC(test = yhat_test2,stat = test$y_faulty, plot = "ROC",AUC=T,main="KNN")

#예측
new.data<-data.frame(fix_time=c(87,85.6), a_speed=c(0.609,0.472), b_speed=c(1.715,1.685),
                     separation=c(242.7,243.4), s_separation=c(657.5,657.9),
                     rate_terms=c(95,95), mpa=c(78,28.8), load_time=c(18.1,18.2),
                     highpressure_time=c(82,60))

knn(xmat_train,new.data,as.factor(y_faulty.train),k=5)

new.data2<-data.frame(test[1:9])

knn(xmat_train,new.data2,as.factor(y_faulty.train),k=5)
