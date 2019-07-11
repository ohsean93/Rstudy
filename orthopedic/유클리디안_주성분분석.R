#유사도 분석
#다차원척도법
#주성분 분석

autoparts<-read.csv("C:/rstudy/11/rtest/autoparts.csv",header = T)
autoparts4<-autoparts[autoparts$prod_no=="45231-P3B750",c(2:11)]

autoparts4$flag <- ifelse(autoparts4$c_thickness<20,3,
                          ifelse(autoparts4$c_thickness>32,2,1))


d<-dist(autoparts4[1:9])
fit<-cmdscale(d)
head(fit)

x<-fit[,1]
y<-fit[,2]

plot(x,y,type = "n")
text(x,y,labels = autoparts4$flag,cex=0.7,col = autoparts4$flag)
