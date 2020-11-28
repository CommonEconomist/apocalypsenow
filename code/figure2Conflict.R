# Figure conflict data
# date created 2018.08.06
# last update  2019.03.13
setwd("~/github/climate-conflict")
library(cshapes)
library(plyr)
library(raster)

# Create raster
load('rawdata/africa.RData') # boundaries continent
r<-raster()                  # one degree
r[]<-1             
cr<-crop(r, extent(africa),snap="out")  # crop raster
shp.r<-rasterize(africa, cr)            # polygon to grid cells
r<-mask(x=cr,mask=shp.r)                # mask
r[]<-1:ncell(r)                         # cell ID
cell_n<-readRDS("data/processed/cells.rds") # included cells
rm(africa, cr, shp.r)

# Conflict data
load('rawdata/ged181.RData')             
p<-ged181[ged181$where_prec<2,]       # 50K accuracy
coordinates(p)<-~longitude+latitude   # spatial points
proj4string(p)<-proj4string(r)        # assign projection
p<-spTransform(p,CRS(projection(r)))  # transform projection
p$cell_n<-cellFromXY(r, p)            # locate cells
p<-p[!(is.na(p$cell_n)),]             # drop NAs
p<-p[p$cell_n %in% cell_n,]           # keep cells in T data
p$period<-ifelse(p$year>=2003, 1, 0)  # period indicator
v<-ddply(data.frame(p),.(cell_n, period), # conflict years
         summarise, n=length(unique(year)))

# Data to raster
r0<-r1<-r
r0[]<-r1[]<-NA                       # set NA
r1[v$cell_n[v$period==1]]<-v$n[v$period==1]                
r0[v$cell_n[v$period==0]]<-v$n[v$period==0]              

# National borders
load('rawdata/iso.rda')               # ISO codes
b<-cshp(date=as.Date("2015-12-31"))   # most recent year
b<-b[b$ISO1AL3 %in% iso,]             # subset

# Plot data
par(mar=c(5,5,2,2),bty='n',las=1,pty='s',cex.axis=2,cex.lab=2,mfrow=c(1,2))

# a) 1989-2002
plot(r0,axes=F,xlab='Latitude',ylab='Longitude',
     main='a) Conflict years 1989-2002',asp=1,
     col=colorRampPalette(c('goldenrod','firebrick','black'))(255))
axis(1,tick=F,line=-1)
axis(2,tick=F,line=-1)
plot(b,add=TRUE,lty=3,border="black")

# b) 2003-2016
plot(r1,axes=F,xlab='Latitude',ylab='Longitude',
     main='b) Conflict years 2003-2017',asp=1,
     col=colorRampPalette(c('goldenrod','firebrick','black'))(255))
axis(1,tick=F,line=-1)
axis(2,tick=F,line=-1)
plot(b,add=TRUE,lty=3,border="black")

## FIN