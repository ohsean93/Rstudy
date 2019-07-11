#계층적 군집분석

x1<-c(8,12,15)
x2<-c(10.13,12,15)

x<-data.frame(c(x1,x2))

d<-dist(x)
plot(hclust(d))


x=sample(1:20,8,T)

plot(hclust(dist(x)))

#autopart 활용

autoparts<-read.csv("C:/rstudy/11/rtest/autoparts.csv",header = T)
autoparts4<-autoparts[autoparts$prod_no=="45231-P3B750",c(2:11)]

autoparts4$flag <- ifelse(autoparts4$c_thickness<20,3,
                          ifelse(autoparts4$c_thickness>32,2,1))

table(autoparts4$flag)

data.scaled3<-scale(autoparts4[,1:9])

d<-dist(data.scaled3)
plot(hclust(d))
