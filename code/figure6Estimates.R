# Bayesian model averaging | Different conflict types
# date created 2018.08.01
# last update  2019.03.19
setwd("~/github/climate-conflict")

# Libraries
library(BMA)
library(data.table)
library(mvtnorm)
source('code/functions.R')

# Prepare data
x<-readRDS("data/x.rds")
x<-na.omit(x)
X<-data.table(incidence.l=x$L.incidence,
              sapply(x[,c('L.W.incidence','temp.d','prec.d',
                          'pop','nlight','nlight.d','eth_pol',
                          'W.temp.d','W.prec.d')],stan))

# Fit models
y=ifelse(x$state!=0,1,0) # civil conflict
m1<-bic.glm(X, y,  glm.family="binomial",strict = FALSE,OR=10^3)
summary(m1,n.models=1,digits=1)

y=ifelse(x$communal!=0,1,0) # communal conflict
m2<-bic.glm(X, y,  glm.family="binomial",strict = FALSE,OR=10^3)
summary(m2,n.models=1,digits=1)

y=ifelse(x$repression!=0,1,0) # repression
m3<-bic.glm(X,y, glm.family="binomial",strict = FALSE,OR=10^3)
summary(m3,n.models=1,digits=1)

# Plot
par(mfrow=c(2,3),cex.main=2.5)
var<-c(expression('Conflict'[t-1]),
       expression(paste(bold(W)," Conflict"[t-1])),
       expression(paste(Delta, 'Temperature')),
       expression(paste(Delta, 'Precipitation')),
       'Population',
       'Night lights',
       expression(paste(Delta, 'Night lights')),
       'Ethnicity',
       expression(paste(bold(W), Delta, ' Temperature')),
       expression(paste(bold(W), Delta, ' Precipitation')))


BMA.plot(m=m1,dvlab='a) State-based',n=1e4)
axis(2,length(var):1,labels=var,las=1,tick=F,cex.axis=1.5,line=-2.5)
BMA.plot(m=m2,dvlab='b) Non-state',n=1e4)
BMA.plot(m=m3,dvlab='c) One-sided violence',n=1e4)

# Drop spatial lag
X<-data.table(incidence.l=x$L.incidence,
              sapply(x[,c('L.W.incidence','temp.d','prec.d',
                          'pop','nlight','nlight.d','eth_pol')],stan))

# Fit models
y=ifelse(x$state!=0,1,0) # civil conflict
m1<-bic.glm(X, y,  glm.family="binomial",strict = FALSE,OR=10^3)

y=ifelse(x$communal!=0,1,0) # communal conflict
m2<-bic.glm(X, y,  glm.family="binomial",strict = FALSE,OR=10^3)

y=ifelse(x$repression!=0,1,0) # repression
m3<-bic.glm(X,y, glm.family="binomial",strict = FALSE,OR=10^3)

# Plot
var<-c(expression('Conflict'[t-1]),
       expression(paste(bold(W)," Conflict"[t-1])),
       expression(paste(Delta, 'Temperature')),
       expression(paste(Delta, 'Precipitation')),
       'Population',
       'Night lights',
       expression(paste(Delta, 'Night lights')),
       'Ethnicity')

BMA.plot(m=m1,dvlab='',n=1e4)
axis(2,length(var):1,labels=var,las=1,tick=F,cex.axis=1.5,line=-2.5)
BMA.plot(m=m2,dvlab='',n=1e4)
BMA.plot(m=m3,dvlab='',n=1e4)

## FIN