#로지스터 회귀 분석

#데이터 가져오기
autoparts<-read.csv("C:/rstudy/11/rtest/autoparts.csv",header = T)
autoparts1<-autoparts[autoparts$prod_no=="90784-76001",c(2:11)]
autoparts2<-autoparts1[autoparts1$c_thickness<1000,]

#grouping하기
autoparts2$y_faulty <- ifelse(((autoparts2$c_thickness<20)|
                                 (autoparts2$c_thickness>32)),1,0)

autoparts2$y_faulty <- as.factor(autoparts2$y_faulty)

#간단한 eda
head(autoparts2)

table(autoparts2$y_faulty)


#로지스터 회귀와 단순회귀 비교
m <- glm(y_faulty~fix_time+a_speed+b_speed+separation+
           s_separation+rate_terms+mpa+load_time+highpressure_time
         , data = autoparts2, binomial(logit))

m1 <- lm(c_thickness~fix_time+a_speed+b_speed+separation+
           s_separation+rate_terms+mpa+load_time+highpressure_time
         , data = autoparts2)

summary(m)
summary(m1)


#훈련, 테스트 셋 나누기
t_index=sample(1:NROW(autoparts2),size = NROW(autoparts2)*0.7)
t_index=sort(t_index)                     #레코드의 선후 유지

train<-autoparts2[t_index,]
test<-autoparts2[-t_index,]

nrow(train);nrow(test)
nrow(train)+nrow(test)==nrow(autoparts2)  #확인하니 잘 나뉘었다.

head(train);head(test)

#모델 형성
m <- glm(y_faulty~fix_time+a_speed+b_speed+separation+
           s_separation+rate_terms+mpa+load_time+highpressure_time
         , data = train, binomial(logit))

m$fitted.values                           # 연속형 결과값

#train 데이터의 예측값 확인
yhat<-ifelse((m$fitted.values>=0.5),1,0)  # 0.5로 우선 해 본다.  

table<-table(real=train$y_faulty,predict=yhat);table  # 결과가 좋지는 않다.


yhat<-ifelse((m$fitted.values>=0.3),1,0)  # 0.3이 더 잘맞는듯하다.

table<-table(real=train$y_faulty,predict=yhat);table

#test셋으로 확인
yhat_test<-predict(m,test,type = "response")  #response-확률값이다.
head(yhat_test,20)  

#library(Epi)
ROC(test=yhat_test,stat = test$y_faulty,plot = "ROC",AUC = T,
    main="Logistices Regression")

new.data<-data.frame(fix_time=c(87,85.6), a_speed=c(0.609,0.472), b_speed=c(1.715,1.685),
                     separation=c(242.7,243.4), s_separation=c(657.5,657.9),
                     rate_terms=c(95,95), mpa=c(78,28.8), load_time=c(18.1,18.2),
                     highpressure_time=c(82,60))

ifelse(predict(m,newdata = new.data,type = "response")>=0.5,1,0)
predict(m,newdata = new.data,type = "response")



#다항 로지스틱
autoparts2$g_class<- as.factor(ifelse(autoparts2$c_thickness<20,1,
                                      ifelse(autoparts2$c_thickness>32,2,3)))
                               

t_index=sample(1:NROW(autoparts2),size = NROW(autoparts2)*0.7)
t_index=sort(t_index)                     #레코드의 선후 유지

train<-autoparts2[t_index,]
test<-autoparts2[-t_index,]

nrow(train);nrow(test)
nrow(train)+nrow(test)==nrow(autoparts2)  #확인하니 잘 나뉘었다.

head(train);head(test)

#install.packages("nnet")
#library(nnet)

m2<-multinom(g_class~fix_time+a_speed+b_speed+separation+
               s_separation+rate_terms+mpa+load_time+highpressure_time
             , data = train)

summary(m2)

head(m2$fitted.values)

yhat_test2<- predict(m2,test)
table2<-table(real=test$g_class,predict=yhat_test2)
table2

(table2[1,1]+table2[2,2]+table2[3,3])/sum(table2)
