#의사결정나무
#install.packages("tree")
#library(tree)
#library(Epi)

#데이터 가져오기
autoparts<-read.csv("C:/rstudy/11/rtest/autoparts.csv",header = T)
autoparts1<-autoparts[autoparts$prod_no=="90784-76001",c(2:11)]
autoparts2<-autoparts1[autoparts1$c_thickness<1000,]

#grouping하기
autoparts2$g_class <- ifelse(autoparts2$c_thickness<20,1,
                              ifelse(autoparts2$c_thickness>32,2,3))

head(autoparts2)

#훈련, 테스트 셋 나누기
t_index=sample(1:NROW(autoparts2),size = NROW(autoparts2)*0.7)
t_index=sort(t_index)

train<-autoparts2[t_index,]
test<-autoparts2[-t_index,]

nrow(train);nrow(test)
nrow(train)+nrow(test)==nrow(autoparts2) #확인하니 잘 나뉘었다.

head(train);head(test)


#의사결정나무 사용 기본 모델 형성
m<-tree(factor(g_class)~fix_time+a_speed+b_speed+separation+
          s_separation+rate_terms+mpa+
          load_time+highpressure_time,data=train)

summary(m)
plot(m)
text(m)

#가지치기
prune.m<-prune.tree(m,method = "misclass")
plot(prune.m)

best.prune.m<-prune.tree(m,best=9)
plot(best.prune.m)
text(best.prune.m)

simple.prune.m<-prune.tree(m,best=4)
plot(simple.prune.m)
text(simple.prune.m)

yhat_test1<-predict(m,test,type = "class")
yhat_test2<-predict(best.prune.m,test,type = "class")
yhat_test3<-predict(simple.prune.m,test,type = "class")


#모델 분석

table1<-table(real=test$g_class,predict=yhat_test1);table1
(table1[1,1]+table1[2,2]+table1[3,3])/sum(table1)

table2<-table(real=test$g_class,predict=yhat_test2);table2
(table2[1,1]+table2[2,2]+table2[3,3])/sum(table2)

table3<-table(real=test$g_class,predict=yhat_test3);table3
(table3[1,1]+table3[2,2]+table3[3,3])/sum(table3)



#의사결정나무 사용 회귀 모델 형성
m<-tree(c_thickness~fix_time+a_speed+b_speed+separation+
          s_separation+rate_terms+mpa+
          load_time+highpressure_time,data=train)

summary(m)
plot(m)
text(m)

yhat_test=predict(m,test)
mse=mean((yhat_test-test$c_thickness)^2)
mse

new.data<-data.frame(fix_time=c(87,85.6), a_speed=c(0.609,0.472), b_speed=c(1.715,1.685),
                     separation=c(242.7,243.4), s_separation=c(657.5,657.9),
                     rate_terms=c(95,95), mpa=c(78,28.8), load_time=c(18.1,18.2),
                     highpressure_time=c(82,60))

predict(m,newdata = new.data)

new.data2<-data.frame(test[1:9])
predict(m,newdata = new.data2)
