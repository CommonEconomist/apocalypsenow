# Bayesian model averaging
# date created 2018.08.01
# last update  2019.03.19
setwd("~/github/climate-conflict")

# Libraries
library(BMA)
library(data.table)
library(mvtnorm)
source('code/functions.R')

x<-readRDS("data/x.rds")
x<-na.omit(x)

# 2) Estimate models
# 2.1) Conflict incidence
# NB - can leave out spatial lag for climate to test robustness
X<-data.table(sapply(x[,c('L.incidence','L.W.incidence','temp.d','prec.d',
                          'pop','nlight','nlight.d','eth_pol',
                          'W.temp.d','W.prec.d')],stan))
y=x$incidence #(639)
m1<-bic.glm(X, y,glm.family=binomial(link = "logit"), 
            strict = FALSE, OR=10^3)
summary(m1,n.models=1,digits=1)

# 2.2) Conflict prevalence
X2<-data.table(sapply(x[,c('baseline','L.W.incidence','temp.d','prec.d',
                          'pop','nlight','nlight.d','eth_pol',
                          'W.temp.d','W.prec.d')],stan))
y=x$outcome
m2<-bic.glm(X2, y, glm.family=quasibinomial(link = "logit"),
            strict = FALSE,OR=10^3)
summary(m2,n.models=1,digits=1)


# 3) Plot results
# Labels
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

# Plot
par(mfrow=c(1,2))
BMA.plot(m=m1,dvlab='a) Conflict incidence',n=1e4)
axis(2,length(var):1,labels=var,las=1,tick=F,cex.axis=1.5,line=-2.5)
BMA.plot(m=m2,dvlab='b) Conflict prevalance',n=1e4)

# 4) Test predictive error (in-sample)
y=x$incidence

# Benchmark model
m0<-bic.glm(X[,c(1:2,5)],y,glm.family="binomial",strict=FALSE,OR=10^3)
yhat0<-predict(m0,X[,c(1:2,5)])
mean(sqrt((yhat0-y)^2))                # 0.31

# Including climate variable
yhat<-predict(m1,X)
mean(sqrt((yhat-y)^2))                 # 0.29

# Without precipitation
m1a<-bic.glm(X[,c(1:3,5)],y,glm.family="binomial",strict=FALSE,OR=10^3)
test<-predict(m1a,X[,c(1:3,5)])
mean(sqrt((test-y)^2))                # 0.29

# 5) Excluding spatial lag climate
X<-data.table(sapply(x[,c('L.incidence','L.W.incidence','temp.d','prec.d',
                          'pop','nlight','nlight.d','eth_pol')],stan))
y=x$incidence #(639)
m1a<-bic.glm(X, y,glm.family=binomial(link = "logit"), 
            strict = FALSE, OR=10^3)
summary(m1a, n.models=1, digits=1)

# 6) Non-linear effects
x$temp.sq<-x$temp.d^2
x$prec.sq<-x$prec.d^2
X<-data.table(sapply(x[,c('L.incidence','L.W.incidence','temp.d','prec.d',
                          'temp.sq', 'prec.sq',
                          'pop','nlight','nlight.d','eth_pol')],stan))
y=x$incidence 
m1b<-bic.glm(X, y,glm.family=binomial(link = "logit"), 
             strict = FALSE, OR=10^3)
summary(m1b, n.models=1, digits=1)

# 7) Test with contemporaneous spatial lag for best model
library(brms)
X<-data.table(incidence=x$incidence,
              sapply(x[,c('L.incidence','W.incidence','temp.d','pop')],stan))
m<-brm(incidence ~ L.incidence + W.incidence + temp.d + pop, X,
       family=bernoulli(link="logit"), iter=1e3, seed=42)
print(m, prob=.5)

## FIN