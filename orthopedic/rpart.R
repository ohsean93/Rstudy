#library(rpart)
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

#의사결정나무 사용 기본 모델 형성
m<-rpart(factor(y_faulty)~fix_time+a_speed+b_speed+separation+s_separation+
           rate_terms+mpa+load_time+ highpressure_time,data=train)
plot(m)
text(m)

plotcp(m)
printcp(m)

m2<-prune(m,cp=m$cptable[5,"CP"])

plot(m2)
text(m2)
fancyRpartPlot(m)


#rattle페키지 fancyRpartPlot
#install.packages("rattle")
#library(rattle)
