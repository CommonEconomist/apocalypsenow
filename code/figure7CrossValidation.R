# Figure cross-validation
# date created 2018.08.28
# last update  2019.03.13
setwd("~/github/climate-conflict")

# Libraries
library(beepr)
library(BMA)
library(data.table)
library(ROCR)
library(caTools)
source('code/functions.R')                                 # functions

# Load data
x<-readRDS("data/x.rds")
x<-na.omit(x)
X<-data.table(sapply(x[,c('L.incidence', 'L.W.incidence',
                          'temp.d','prec.d', 
                          'pop', 'nlight', 'nlight.d','eth_pol',
                          'W.temp.d','W.prec.d')],stan))
y<-x$incidence                                             # outcome

# Leave-one-out cross-validation (~5m)
yhat0<-yhat<-rmse<-rmse0<-c()                              # vectors
start<-Sys.time()
for(i in 1:length(y)){                                     # cross-validate
  print(i)
  set.seed(42)
  m<-bic.glm(X[-i, c(1:5)], y[-i],
             glm.family="binomial",strict = FALSE, OR=10^3)
  yhat[i]<-predict(m,newdata=X[c(i, 1000)])[1]              # bug in code
  rmse[i]<-sqrt((yhat[i]-y[i])^2)
  
  
  m<-bic.glm(X[-i,c(1:2, 5)],y[-i], 
             glm.family="binomial", strict = FALSE, OR=10^3)
  yhat0[i]<-predict(m,newdata=X[c(i,1000)], c(1:2,5))[1]     # bug in code
  rmse0[i]<-sqrt((yhat[i]-y[i])^2)
}
Sys.time()-start
beep(3)

# 2) Summary statistics
mean(sqrt((yhat-y)^2))  # 0.29
mean(sqrt((yhat0-y)^2)) # 0.31

# 3.1) Precision-recall curves
pred<-prediction(yhat,y)
f<-performance(pred,"f")
mean(as.numeric(unlist(f@y.values)),na.rm=TRUE)    # 0.48
pr1<-performance(pred,"prec","rec")
trapz(pr1@x.values[[1]][-1],pr1@y.values[[1]][-1]) # 0.58

pred<-prediction(yhat0,y)
f<-performance(pred,"f")
mean(as.numeric(unlist(f@y.values)),na.rm=TRUE)    # 0.46
pr0<-performance(pred,"prec","rec")
trapz(pr0@x.values[[1]][-1],pr0@y.values[[1]][-1]) # 0.51

# 3.2) Plot curves
par(mar=c(5,5,2,2),pty='s',bty='n',las=1,cex.lab=2,cex.axis=1.5)
plot(pr1,xaxis.col="white",yaxis.col="white",ylim=c(0,1),col='firebrick',
     xlab="Recall",ylab="Precision",lwd=2)
plot(pr0,add=TRUE,lty=2,lwd=2)
abline(h=sum(y)/length(y),lty=2)
legend("bottomleft",legend=c("Climate + controls","Controls only"),
       col=c("Firebrick3","black"),
       text.width=.4,lty=c(1,2),bty="n",lwd=5,
       y.intersp=c(0.4),x.intersp=c(.1),xjust=1,cex=1.5)

save.image("output/loo.rda")
## FIN