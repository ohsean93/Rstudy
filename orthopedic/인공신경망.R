#데이터 가져오기
autoparts<-read.csv("C:/rstudy/11/rtest/autoparts.csv",header = T)
autoparts1<-autoparts[autoparts$prod_no=="90784-76001",c(2:11)]
autoparts2<-autoparts1[autoparts1$c_thickness<1000,]

#grouping하기
autoparts2$g_class <-as.factor( ifelse((autoparts2$c_thickness<20),1,
                             ifelse(autoparts2$c_thickness>32,2,3)) )

#훈련, 테스트 셋 나누기
t_index=sample(1:NROW(autoparts2),size = NROW(autoparts2)*0.7)
t_index=sort(t_index)

train<-autoparts2[t_index,]
test<-autoparts2[-t_index,]

nrow(train);nrow(test)
nrow(train)+nrow(test)==nrow(autoparts2) #확인하니 잘 나뉘었다.

head(train);head(test)

#인공신경망 구성
#install.packages("nnet")
#library(nnet)

m<-nnet(g_class~fix_time+a_speed+b_speed+separation+
          s_separation+rate_terms+mpa+
          load_time+highpressure_time, data=train,size=10)

yhat_test1<-predict(m,test,type = "class")
table1<-table(real=test$g_class,predict=yhat_test1);table1
x1<-c(0,0,0)
table1<-cbind(x1,table1)

(table1[1,1]+table1[2,2]+table1[3,3])/sum(table1)

library(reshape2)
library(devtools)

source_url("https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r")
plot(m)


new.data<-data.frame(fix_time=c(87,85.6), a_speed=c(0.609,0.472), b_speed=c(1.715,1.685),
                     separation=c(242.7,243.4), s_separation=c(657.5,657.9),
                     rate_terms=c(95,95), mpa=c(78,28.8), load_time=c(18.1,18.2),
                     highpressure_time=c(82,60))

predict(m,newdata = new.data,type = "class")



predict(m,newdata = new.data2,type = "class")
