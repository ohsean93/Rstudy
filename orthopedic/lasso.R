#lasso
##library(glmnet)

#데이터 프레임 형성 및 전처리
autoparts=read.csv("autoparts.csv",header = T)
autoparts1=autoparts[autoparts$prod_no=="90784-76001",c(2:11)]
autoparts2=autoparts1[autoparts1$c_thickness<1000,]
autoparts3=autoparts2[autoparts2$highpressure_time<1000,]


xmat=as.matrix(autoparts2[1:9])
yvec=autoparts2$c_thickness

fit.lasso=glmnet(x=xmat,y=yvec,alpha = 1,nlambda = 100)
fit.lasso.cv=cv.glmnet(x=xmat,y=yvec,nfolds = 10,alpha=1,lambda = fit.lasso$lambda)

plot(fit.lasso.cv)

fit.lasso.cv$lambda.min
min(fit.lasso.cv$cvm)


fit.lasso.param=fit.lasso.cv$lambda.min

fit.lasso.param
fit.lasso.tune=glmnet(x=xmat,y=yvec,alpha = 1,lambda = fit.lasso.param)

fit.lasso.tune
coef(fit.lasso.tune)
