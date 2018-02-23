getwd()
setwd("C:/Repository")
podaci=read.table("vrniz.txt")
niz=as.numeric(podaci[,1])
n=length(niz)
ts.plot(niz,xlab="Time",ylab="Podaci",main="Put vremenskog niza")

#seasonality
duljina=length(niz)
par(mfrow=c(1,2))
plot(niz[0:(duljina-4)],niz[5:duljina],main="Vremenski niz")
#assuming seasonality
par(mfrow=c(2,1))
acf(niz,main="Vremenski niz") 
#niz.trend=diff(niz)
#len.trend=length(niz.trend)
#plot(niz.trend[0:(len.trend-4)],niz.trend[5:(len.trend)],col="gray54",main="Diferencirani vremenski niz") 

#seasonality 4
#removing seasonality


xt=ts(niz,start=1,freq=4)
xt
xt2=matrix(xt,byrow=T,ncol=4)
xt2


#quarterly average 
m=rowMeans(xt2)

#seasonal components

s=colMeans(xt2-m)

niz2=niz-s
ts.plot(niz2, main="Vremenski niz bez sezonalnost")

#removing trend
t=0:(n-1)
rez=lm(niz2~I(t))
summary(rez)

lines(t,rez$fit,col="red")

par(mfrow=c(3,1))
ts.plot(niz2, main="Vremenski niz s ukolnjenom sezonalnosti")
ts.plot(niz2, main="Procjena podataka polinomom 1. stupnja")
lines(t,rez$fit,col="red")
ts.plot(niz3,main="Transformirani niz")

#residuals
niz3=rez$res

par(mfrow=c(2,1))
ts.plot(niz2)
lines(t,rez$fit,col="red")
ts.plot(niz3,main="Transformirani niz")

par(mfrow=c(1,1))
ts.plot(niz3,main="Transformirani niz")
abline(0,0,col="red")


par(mfrow=c(2,1))
acf(niz3, main="Transformirani niz")
pacf(niz3, main="Transformirani niz")

install.packages("tseries")
library(tseries)
adf.test(niz3,alternative = c("stationary"))
adf.test(rez$res,alternative = c("stationary"))


#acf => MA(1) model
#pacf => AR(3)  model

#yule-walker estimation model
yw=ar(niz3,method="yule-walker")
summary(yw)
#AR(3) model


#MA model <5
arma_AIC=function(x,n){
m=matrix(0,(n+2)*(n+1)/2,3)
k=0
for(i in 0:n){
	for(j in 0:(n-i)){
	tt=arima(x,order=c(0,0,j),method="ML",include.mean=F)
	k=k+1;
	m[k,]=c(i,j,tt$aic);
	print(m[k,])
	}
	}
	u=min(m[,3])
	m[m[,3]==u,]
}
arma_AIC(niz3,5)

#MA(1) AIC is 5228,572

#ARMA model
arma2_AIC=function(x,n){
m=matrix(0,(n+2)*(n+1)/2,3)
k=0
for(i in 0:n){
	for(j in 0:(n-i)){
	tt=arima(x,order=c(i,0,j),method="ML",include.mean=F)
	k=k+1;
	m[k,]=c(i,j,tt$aic);
	print(m[k,])
	}
	}
	u=min(m[,3])
	m[m[,3]==u,]
}
arma2_AIC(niz3,3)

#ARMA(1,1) AIC is 5216,386 

#model checking 
ar2=arima(niz3,order=c(2,0,0),method="ML", include.mean=F)
ar3=arima(niz3,order=c(3,0,0),method="ML", include.mean=F)
arma21=arima(niz3,order=c(2,0,1),method="ML", include.mean=F)
arma12=arima(niz3,order=c(1,0,2),method="ML", include.mean=F)
arma11=arima(niz3,order=c(1,0,1),method="ML", include.mean=F)
ma2=arima(niz3,order=c(0,0,2),method="ML", include.mean=F)
ma3=arima(niz3,order=c(0,0,3),method="ML", include.mean=F)
ma1=arima(niz3,order=c(0,0,1),method="ML", include.mean=F)
ar1=arima(niz3,order=c(1,0,0),method="ML", include.mean=F)
ar2$aic
ar3$aic
arma21$aic
ma2$aic
arma12$aic
arma11$aic
ma3$aic
ma1$aic
ar1$aic

#AR(3) model

arma=arima(niz3,order=c(3,0,0),method="ML", include.mean=F)
arma$coef
arma #sigma2
ostaci=arma$resid


par(mfrow=c(1,2))
#acf(ostaci[!is.na(ostaci)])
acf(ostaci, main="ACF ostataka")
#ts.plot(ostaci[!is.na(ostaci)])
qqnorm(ostaci)
qqline(ostaci)
Box.test(ostaci,lag=26,type="Ljung-Box") # pv=0.7787

#forecasting next 5 values 

ostaci.pred=predict(arma,n.ahead=5)
ostaci.pred$pred


x0=data.frame(t=(1200+(1:5)/4))
x0
ost.tr.pred=as.numeric(predict(rez,x0))
ost.tr.pred

#forecast
niz.pred=ost.tr.pred+ostaci.pred$pred
niz.pred #forecasted values

#upper and lower confidence intervals
pred.d=niz.pred-1.96*ostaci.pred$se
pred.g=niz.pred+1.96*ostaci.pred$se
pred.d
pred.g #intervals


mini=min(niz,pred.d)
maxi=max(niz,pred.g)
podaci.pred=ts(niz.pred,start=1200,freq=4)
podaci.pred.d=ts(pred.d,start=1200,freq=4)
podaci.pred.g=ts(pred.g,start=1200,freq=4)
ts.plot(niz,xlim=c(1100,1205),ylim=c(mini,maxi), main="Graf procjenjenih vrijednosti")
lines(podaci.pred,col="red")
niz4=c(niz,podaci.pred)
for (i in 1:5){
points(1200+i,niz4[i+1200],col="red")
#lines(podaci.pred,col="red")

points(podaci.pred,col="red")
lines(podaci.pred.d,col="green")
lines(podaci.pred.g,col="green")


par(mfrow=c(1,1))
novi=niz4[1100:1200]
plot(niz,type='l',xlim=c(1100,1206),main=("Graf procijenjenih vrijednosti s predikcijskim intervalima"))
#podpred=ts(prniz,start=1200)
podpredd=ts(pred.d,start=1200)
podpredg=ts(pred.g,start=1200)
#lines(podpred,lwd=2,lty=3)
lines(podpredd,col="green",lwd=2,lty=3 )
lines(podpredg,col="green",lwd=2,lty=3)
for (i in 1:5){
points(1200+i,niz4[i+1200],col="red")}
