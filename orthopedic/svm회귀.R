#cvm회구
#데이터 가져오기
autoparts<-read.csv("C:/rstudy/11/rtest/autoparts.csv",header = T)
autoparts1<-autoparts[autoparts$prod_no=="90784-76001",c(2:11)]
autoparts2<-autoparts1[autoparts1$c_thickness<1000,]
autoparts3<-autoparts2[autoparts2$highpressure_time<1000,]

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

#파라미터 최적값 찾기
#install.packages("e1071")
#library(e1071)
#tune.svm(c_thickness~fix_time+a_speed+b_speed+separation+
#           s_separation+rate_terms+mpa+load_time+
#           highpressure_time,
#         data = autoparts2,gamma = 2^(-1:1),cost = 2^(2:4))


m<- svm(c_thickness~fix_time+a_speed+b_speed+separation+
          s_separation+rate_terms+mpa+load_time+
          highpressure_time,
        data = train,gamma = 2,cost = 16)


summary(m)

yhat_test<-predict(m,test)
plot(test$c_thickness,y=yhat_test,main = "SVR")
mse<- mean((test$c_thickness-yhat_test)^2)
mse

#다중회귀와의 비교
lm1<- lm(c_thickness~fix_time+a_speed+b_speed+separation+
          s_separation+rate_terms+mpa+load_time+
          highpressure_time, data = train)

summary(lm1)
yhat_test2<-predict(lm1,test)
plot(test$c_thickness,y=yhat_test2,main = "ML")
mse2<- mean((test$c_thickness-yhat_test2)^2)
mse2

new.data<-data.frame(fix_time=c(87,85.6), a_speed=c(0.609,0.472), b_speed=c(1.715,1.685),
                     separation=c(242.7,243.4), s_separation=c(657.5,657.9),
                     rate_terms=c(95,95), mpa=c(78,28.8), load_time=c(18.1,18.2),
                     highpressure_time=c(82,60))

predict(m,newdata = new.data)

