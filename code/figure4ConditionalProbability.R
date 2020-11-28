# Figure conditional probability
# date created 2018.08.06
# last update  2019.03.20
setwd("~/github/climate-conflict")
x<-readRDS("data/x.rds")
library(FSA)

prop.table(table(x$incidence,x$L.incidence),2)

# 1.1) Empirical patterns
t<-seq(.1,.7,length=100)               # range temperature increase
P<-c();lwr<-c();upr<-c()        
n<-c();v<-c()
y<-x[x$L.incidence==0,]                # subset: no previous conflict

for(i in 1:length(t)){
  print(i)
  
  # Calculate probability
  MA<-ifelse(y$temp.d>=t[i],1,0)      
  A<-prop.table(table(y$incidence,MA),2)
  P[i]=A[2,2]
  
  # 50% uncertainty interval
  n[i]<-sum(MA)
  v[i]<-sum(y$incidence[MA==1])
  ui<-binCI(v[i],n[i],conf.level=.5,type="exact")
  lwr[i]<-ui[,1];upr[i]<-ui[,2]
}

# 1.2) Plot results
par(bty="n",las=1,cex.lab=2,cex.axis=2,mar=c(5,6,0,0),pty='m')
plot(t,P,type="n",ylim=c(.05,.3),xlim=c(.1,.7),axes=FALSE,
     ylab="Pr(violence)",
     xlab="Minimum increase temperature anomaly")
polygon(c(rev(t),t),c(rev(lwr),upr),col="grey80",border=NA) #UI
lines(t,P,lwd=2)
axis(1,tick=FALSE);axis(2,tick=FALSE,line=-1.5)

## FIN