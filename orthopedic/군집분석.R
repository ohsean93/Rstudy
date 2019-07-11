
#군집분석
iris2<-iris[3:4]

data.scaled <- scale(iris2)
str(data.scaled)
summary(iris2)
summary(data.scaled)

head(iris2);head(data.scaled)

data.cluster<-kmeans(data.scaled,centers = 3)
data.cluster

plot(data.scaled,col=data.cluster$cluster)
points(data.cluster$centers,col=1:3,pch=8,cex=2)

#autopart 활용

autoparts<-read.csv("C:/rstudy/11/rtest/autoparts.csv",header = T)
autoparts3<-autoparts[autoparts$prod_no=="45231-3B610",c(2:11)]



autoparts3$flag <- ifelse(autoparts3$c_thickness<20,3,
                          ifelse(autoparts3$c_thickness>32,2,1))

table(autoparts3$flag)

data.scaled2<-scale(autoparts3[,1:9])

data.cluster2<-kmeans(data.scaled2,centers = 3)
data.cluster2

plot(data.scaled2,col=data.cluster2$cluster)
points(data.cluster2$centers,col=1:3,pch=8,cex=2)

table(real=autoparts3$flag,predict=data.cluster2$cluster)
