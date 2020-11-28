# Figure cross-country heterogeneity
# date created 2019.03.13
# last update  2019.07.31
setwd("~/github/climate-conflict")
library(plyr)
library(data.table)

# Prepare data
x<-readRDS("data/x.rds")
x<-na.omit(x)

x[, `:=` (t.iso = mean(temp.d)), by =iso]
x[, `:=` (t.d = temp.d - t.iso)]
d<-ddply(x, .(iso), summarise, 
         change=mean(temp.d),
         conflict=sum(incidence)/length(iso),
         J=length(iso))

# Plot
par(mar=c(5,5,2,2), pty='s', mfrow=c(1,2), bty='n', 
    las=1, cex.axis=2, cex.lab=1.7, cex.main=2.5)

# Bivariate
plot(conflict ~ change, d, type='n', axes=F, 
     ylab="Cells with conflict (%)", 
     xlab="Average change temperature anomaly",
     main="a.")
text(d$change, d$conflict, labels=d$iso )
abline(lm(conflict ~ change, d), col='black', lwd=1.5)
axis(1,tick=F)
axis(2, tick=F, line=-2)

# Density
plot(density(x$t.d[x$incidence==0]),lty=3, lwd=1.5, col='grey50' ,axes=F,
     ylab="Density", xlab="Temperature change (relative to country mean)",
     main='b.')
lines(density(x$t.d[x$incidence==1]), lwd=2)
lines(density(x$t.d[x$incidence==1 & x$L.incidence==0]), lty=2, lwd=2)
axis(1, tick=F)

legend(-.25, 8.2,
       c("No conflict", "Conflict incidence", "Conflict onset"),  cex=1,
       lty=c(3, 1, 2), lwd=c(2, 2, 2),col=c("grey50","black", "black"),
       bty="n", y.intersp=c(.5), x.intersp=c(.2))

## FIN