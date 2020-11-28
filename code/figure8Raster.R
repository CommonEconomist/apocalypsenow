# Figure cross-validation
# date created 2018 08 28
# last update  2019 03 13
setwd("~/github/climate-conflict")
load('output/loo.rda')

# National boundaries
require(cshapes)
load('rawdata/iso.rda')               # ISO codes
b<-cshp(date=as.Date("2015-12-31"))   # most recent year
b<-b[b$ISO1AL3 %in% iso,]             # subset

# Use raster
t<-readRDS("data/processed/temperature.rds")
r.d<-t[[269]]
r.d[]<-NA                              # set NA

# Plot
par(mar=c(5,5,3,4),bty='n',las=1,pty='s',
    cex.main=2,cex.axis=2,cex.lab=2,mfrow=c(1,2))

r.d[x$cell_n]<-yhat-y                    # error
plot(r.d,axes=F,xlab='Latitude',ylab='Longitude',
     main='a) Prediction error',
     col=colorRampPalette(c('steelblue4','steelblue',
                            'grey80','grey70',
                            'firebrick3','firebrick4'))(255))
axis(1,tick=F,line=-1)
axis(2,tick=F,line=-1)
plot(b,add=TRUE,lty=3,border="black")

r.d[x$cell_n]<-yhat-yhat0              # prediction difference
plot(r.d,axes=F,xlab='Latitude',ylab='Longitude',
     main='b) Difference in predicted values',
     col=colorRampPalette(c('steelblue4','steelblue',
                            'grey80','grey70',
                            'firebrick3','firebrick4'))(255))
axis(1,tick=F,line=-1)
axis(2,tick=F,line=-1)
plot(b,add=TRUE,lty=3,border="black")

## FIN