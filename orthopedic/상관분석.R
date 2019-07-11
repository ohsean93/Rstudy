#비지도 상관분석, 군집분석
#library(corrgram)

cor(iris$Sepal.Length,iris$Sepal.Width)

corrgram(iris,upper.panel=panel.pie)#계수비례 원 그래프
corrgram(iris,upper.panel=panel.shade)#선이 추가된 채움
corrgram(iris,upper.panel=panel.pts)#플롯이랑 같이 형성
corrgram(iris,upper.panel=panel.fill)#선없이 채움
corrgram(iris,upper.panel=panel.bar)#계수값 만큼 채움/양수는 위에서, 음수는 아래에서
corrgram(iris,upper.panel=panel.ellipse)#상관 곡선
corrgram(iris,upper.panel=panel.conf)#색없는 숫자
corrgram(iris,upper.panel=panel.cor)#색있는 숫자


#데이터 가져오기
autoparts<-read.csv("C:/rstudy/11/rtest/autoparts.csv",header = T)
autoparts1<-autoparts[autoparts$prod_no=="90784-76001",c(2:11)]
autoparts2<-autoparts1[autoparts1$c_thickness<1000,]

#단일 상관분석
cor(autoparts2$separation,autoparts2$s_separation)

cor(autoparts2)

symnum(cor(autoparts2))



