#install.packages("ada")
#library(ada)


iris[iris$Species!="setosa",]->iris2
n<-dim(iris2)[1]

trind<-sort(sample(1:n,floor(0.6*n),F))
teind<-setdiff(1:n, trind)

iris2[,5]<-(as.numeric(iris2[,5])-1)

gdis<-ada(Species~.,data=iris2[trind,],iter=20,nu=1,type="discrete")
gdis2<-addtest(gdis,iris2[teind,-5],iris2[teind,5])
varplot(gdis2)
plot(gdis2)
pairs(gdis2,iris2[trind,-5],maxvar = 4)
