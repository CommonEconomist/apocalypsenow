# Robustness checks | reported in table 2
# NB - rows 1-2 are taken from figure 5 for comparison
# date created 2019.03.19
# last update  2019.07.23
setwd("~/github/climate-conflict")
library(BMA)
library(data.table)
source('code/functions.R')
x<-readRDS("data/x.rds")
x<-na.omit(x)

# ROW 3: Longer benchmark period: incidence
X<-data.table(sapply(x[,c('L.incidence','L.W.incidence','temp.da','prec.da',
                          'pop','nlight','nlight.d','eth_pol',
                          'W.temp.da','W.prec.da')],stan))
y=x$incidence 
m1<-bic.glm(X, y,glm.family=binomial(link = "logit"), 
            strict = FALSE, OR=10^3)
summary(m1,n.models=1,digits=1) 

# ROW 4: Longer benchmark period: prevalence
X<-data.table(sapply(x[,c('baseline','L.W.incidence','temp.d','prec.d',
                           'pop','nlight','nlight.d','eth_pol',
                           'W.temp.d','W.prec.d')],stan))
y=x$outcome
m2<-bic.glm(X, y, glm.family=quasibinomial(link = "logit"),
            strict = FALSE,OR=10^3)
summary(m2,n.models=1,digits=1) 

# ROW 9: Conflict onset
X<-data.table(sapply(x[,c('L.W.incidence','temp.d','prec.d',
                          'pop','nlight','nlight.d','eth_pol',
                          'W.temp.d','W.prec.d')],stan))
y=x$onset 
m3<-bic.glm(X, y, glm.family=binomial(link = "logit"),
            strict = FALSE,OR=10^3)
summary(m3, n.models=1, digits=1) 

y<-ifelse(x$incidence==1 & x$L.incidence==0, 1,
          ifelse(x$incidence==0 & x$L.incidence==0, 0, NA))
m3a<-bic.glm(X, y, glm.family=binomial(link = "logit"),
            strict = FALSE,OR=10^3)
summary(m3a, n.models=1, digits=1) 


# ROW 10: Conflict events (not reported)
X<-data.table(sapply(x[,c('L.conflicts','L.W.incidence','temp.d','prec.d',
                          'pop','nlight','nlight.d','eth_pol',
                          'W.temp.d','W.prec.d')],stan))
y=x$conflicts 
m4<-bic.glm(X, y, glm.family=quasipoisson(link = "log"),
            strict = FALSE, OR=10^3)
summary(m4, n.models=1, digits=1)

# ROW 10: Conflict events | excluding outliers
x2<-x[x$conflicts<=200,]
X<-data.table(sapply(x2[,c('L.conflicts','L.W.incidence','temp.d','prec.d',
                          'pop','nlight','nlight.d','eth_pol',
                          'W.temp.d','W.prec.d')],stan))
y=x2$conflicts 
m5<-bic.glm(X, y, glm.family=quasipoisson(link = "log"),
            strict = FALSE, OR=10^3)
summary(m5, n.models=1, digits=1)

# ROW 11: Change conflict prevalance | update 2019.03.28
X<-data.table(sapply(x[,c('baseline','L.W.incidence','temp.d','prec.d',
                          'pop','nlight','nlight.d','eth_pol',
                          'W.temp.d','W.prec.d')],stan))
y=x$risk.g
m14<-bic.glm(X, y, glm.family=gaussian, strict = FALSE, OR=10^3)
summary(m14, n.models=1, digits=1)

# ROW 12: Including country indicators
x$iso<-factor(x$iso)
X<-data.table(iso=x$iso,
              sapply(x[,c('L.incidence','L.W.incidence','temp.d','prec.d',
                          'pop','nlight','nlight.d','eth_pol',
                          'W.temp.d','W.prec.d')],stan))
y=x$incidence 
m6<-bic.glm(X, y,  glm.family=binomial(link = "logit"),
            strict = FALSE,OR=10^3, factor.type=T)
summary(m6,n.models=1,digits=1)

# ROW 12: Including country indicators
X<-data.table(iso=x$iso,
               sapply(x[,c('baseline','L.W.incidence','temp.d','prec.d',
                           'pop','nlight','nlight.d','eth_pol',
                           'W.temp.d','W.prec.d')],stan))
y=x$outcome
m7<-bic.glm(X, y, glm.family=quasibinomial(link = "logit"),
            strict = FALSE,OR=10^3, factor.type=T)
summary(m7,n.models=1,digits=1)


# ROW 7: 2-degree aggregation: incidence
x<-readRDS("data/x2.rds")
x<-na.omit(x)
X<-data.table(sapply(x[,c('L.incidence','L.W.incidence','temp.d','prec.d',
                          'pop','nlight','nlight.d','eth_pol',
                          'W.temp.d','W.prec.d')],stan))
y=x$incidence 
m8<-bic.glm(X, y,glm.family=binomial(link = "logit"), 
            strict = FALSE, OR=10^3)
summary(m8,n.models=1,digits=1)

# ROW 8: 2-degree aggregation: prevalence
X<-data.table(sapply(x[,c('baseline','L.W.incidence','temp.d','prec.d',
                           'pop','nlight','nlight.d','eth_pol',
                           'W.temp.d','W.prec.d')],stan))
y=x$outcome
m9<-bic.glm(X, y, glm.family=quasibinomial(link = "logit"),
            strict = FALSE,OR=10^3)
summary(m9,n.models=1,digits=1)

# ROW 5: Shorter period: incidence 
x<-readRDS("data/x-alt.rds")
x<-na.omit(x)
X<-data.table(sapply(x[,c('L.incidence','L.W.incidence','temp.d','prec.d',
                          'pop','nlight','nlight.d','eth_pol',
                          'W.temp.d','W.prec.d')],stan))
y=x$incidence 
m10<-bic.glm(X, y,glm.family=binomial(link = "logit"), 
            strict = FALSE, OR=10^3)
summary(m10,n.models=1,digits=1)

# ROW 6: Shorter period: prevalence
X<-data.table(sapply(x[,c('baseline','L.W.incidence','temp.d','prec.d',
                           'pop','nlight','nlight.d','eth_pol',
                           'W.temp.d','W.prec.d')],stan))
y=x$outcome
m11<-bic.glm(X, y, glm.family=quasibinomial(link = "logit"),
            strict = FALSE,OR=10^3)
summary(m11,n.models=1,digits=1)

# ROW 14: cross-section time-series: moving average
x<-readRDS("data/csts.rds")
x<-na.omit(x)
x<-x[x$year>=2003,] # 2003-14
X<-data.table(year=factor(x$year),
              sapply(x[,c('L.incidence','L.W.incidence','temp.ma','prec.ma',
                          'pop','nlight.d','eth_pol')], stan))
y=x$incidence
m12<-bic.glm(X, y,  glm.family=binomial(link = "logit"),
            strict = FALSE, OR=10^3, factor.type=T)
summary(m12, n.models=1, digits=2)

# ROW 15: cross-section time series: annual deviation
X<-data.table(year=factor(x$year),
              sapply(x[,c('L.incidence','L.W.incidence','temp.da','prec.da',
                          'pop','nlight','nlight.d','eth_pol')],stan))
m13<-bic.glm(X, y,  glm.family=binomial(link = "logit"),
            strict = FALSE,OR=10^3, factor.type=T)
summary(m13, n.models=1, digits=2)

# ROW 16: Accounting for long-term trend | update 2019.07.22
x[, `:=`(temp.avg = mean(temp.da, na.rm=TRUE), 
         prec.avg = mean(prec.da, na.rm=TRUE)), by=cell_n]
X<-data.table(year=factor(x$year),
              sapply(x[,c('L.incidence','L.W.incidence',
                          'temp.da','prec.da','temp.avg', 'prec.avg',
                          'pop','nlight','nlight.d','eth_pol')],stan))
m15<-bic.glm(X, y,  glm.family=binomial(link = "logit"),
             strict = FALSE,OR=10^3, factor.type=T)
summary(m15, n.models=1, digits=2)

## FIN