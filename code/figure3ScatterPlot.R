# Figure scatter plot
# date created 2018.08.01
# last update  2019.03.13
setwd("~/github/climate-conflict")
x<-readRDS("data/x.rds")
par(mar=c(5,4,5,2),bty='n',pty='m',las=1,cex.lab=2,cex.axis=1.5,pty='s')

# Plot
plot(x$temp.d[x$incidence==0],
     x$prec.d[x$incidence==0]
     ,cex=.5,pch=4,col="#00000033",axes=F,
     xlab='Change temperature anomaly',ylab='Change precipitation anomaly')

points(x$temp.d[x$incidence==1],
       x$prec.d[x$incidence==1],pch=18,cex=.75,col='firebrick3')

abline(v=mean(x$temp.d,na.rm=T),lty=3)    # mean temperature
abline(h=mean(x$prec.d,na.rm=T),lty=3)    # mean precipitation
axis(1,tick=F)                            # axis
axis(2,tick=F,line=-.5)

rug(x$temp.d[x$incidence==1],side=1,ticksize=.02,lwd=.2) # observations
rug(x$prec.d[x$incidence==1],side=2,ticksize=.02,lwd=.2)

## FIN