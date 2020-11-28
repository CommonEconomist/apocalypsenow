# Summary statistics
# date created 2019.03.13
# last update  2019.03.20
setwd("~/github/climate-conflict")

# Load data
x<-readRDS("data/x.rds")
x<-na.omit(x)
x<-x[,c('incidence', 'outcome', 
        'L.incidence', 'L.W.incidence', 
        'temp.d', 'prec.d',
        'pop','nlight','nlight.d','eth_pol')]

# Table
ds<-function(x, na.rm=TRUE){
  result <- c(Mean=mean(x,na.rm=na.rm),
              SD=sd(x, na.rm=na.rm),
              N=length(x))
}

stats<-data.frame(t(sapply(x,ds)))
stats<-data.frame(t(sapply(x[x$incidence==1,],ds)))
stats<-data.frame(t(sapply(x[x$incidence==0,],ds)))

# t-test
library(BayesianFirstAid)
set.seed(42);bayes.t.test(temp.d~incidence,x)
set.seed(42);bayes.t.test(prec.d~incidence,x)

## FIN