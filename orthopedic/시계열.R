head(AirPassengers)

plot(AirPassengers)

apts<- ts(AirPassengers,frequency = 12)
f<- decompose(AirPassengers)

plot(f)


fit<-arima(AirPassengers,order = c(1,0,0),list(order=c(2,1,0),period=12))
fore<- predict(fit,n.ahaed=24)

U<- fore$pred+2*fore$se
L<- fore$pred-2*fore$se
ts.plot(AirPassengers,fore$pred,U,L,col=c(1,2,4,4),lty=c(1,1,2,2))
