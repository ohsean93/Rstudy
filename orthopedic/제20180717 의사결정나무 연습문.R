### **연습문제**

library(rpart)
# 데이터 준비
autoparts <- read.csv("C:/rstudy/11/rtest/autoparts.csv", header = T)
autoparts1 <- autoparts[autoparts$prod_no == "90784-76001", c(2:11)]
autoparts2 <- autoparts1[autoparts1$c_thickness < 1000,]

autoparts2$y_faulty <- ifelse((autoparts2$c_thickness < 20)|(autoparts2$c_thickness> 32),1, 0)

t_index <- sample(1:nrow(autoparts2), size = nrow(autoparts2)*0.7)
train <- autoparts2[t_index,]
test <- autoparts2[-t_index,]

nrow(train);nrow(test)

# 모델 생성
part.m <- rpart(factor(y_faulty) ~ fix_time + a_speed + b_speed + separation + s_separation +
                  rate_terms + mpa + load_time + highpressure_time, data = train)
plot(part.m)
text(part.m)

# 제일 적당한 가지 수 설정
printcp(part.m)
plotcp(part.m)

# 가지치기
ptree <- prune(part.m,cp = part.m$cptable[5,"CP"])
plot(ptree)
text(ptree)

# 예측하기
pred <- predict(ptree, test, type = "class")
library(caret)
confusionMatrix(pred, factor(test$y_faulty)) # 분류표와 정확도 검사하기
# caret package 필요

