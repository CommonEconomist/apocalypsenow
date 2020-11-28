# Figure climate data
# date created 2018.08.06
# last update  2019.03.13
setwd("~/github/climate-conflict")
library(plyr)
library(cshapes)

# Prepare data
t<-readRDS("data/processed/temperature.rds")  # temperature
m0<-calc(t[[240:253]],fun=mean)               # average: 1989-2003
m<-calc(t[[254:268]],fun=mean)                # average 2003-2017
m.d<-m-m0                                     # difference in anomaly
cell_n<-Which(m.d>0, cells=TRUE)              # cell numbers 

r<-readRDS("data/processed/precipitation.rds")# precipitation
r.d<-m.d
r.d[r$cell_n]<-r$prec.d.V1                 # issue with variable name

# National borders
load('rawdata/iso.rda')               # ISO codes
b<-cshp(date=as.Date("2015-12-31"))   # most recent year
b<-b[b$ISO1AL3 %in% iso,]             # subset

#  Plot
par(mar=c(5,5,3,4),bty='n',las=1,pty='s',cex.axis=2,cex.lab=2,mfrow=c(1,2))

# Temperature
plot(m.d,axes=F,xlab='Latitude',ylab='Longitude',main='a) Temperature',
     col=colorRampPalette(c('chartreuse4','goldenrod','firebrick'))(255))
axis(1,tick=F,line=-1)
axis(2,tick=F,line=-1)
plot(b,add=TRUE,lty=3,border="black")

# Precipitation
plot(r.d,axes=F,xlab='Latitude',ylab='Longitude',main='b) Precipitation',
     col=colorRampPalette(
       c('firebrick','goldenrod','olivedrab3','chartreuse4'))(255))
axis(1,tick=F,line=-1)
axis(2,tick=F,line=-1)
plot(b,add=TRUE,lty=3,border="black")

## FIN