# Functions
# last update 2018.08.02

# Standardize input variables
stan<-function(x){(x-mean(x,na.rm=TRUE))/(2*sd(x,na.rm=TRUE))} 

# Custom breaks histogram
h<-function(x){
  diff(range(x,na.rm=TRUE)) / (2 * IQR(x,na.rm=TRUE) / length(x)^(1/3))
}

# Plot results BMA
BMA.plot<-function(m, dvlab=NULL, n){
  par(mar=c(8,8,4,0.5),xpd=F,bty='n',las=1,                # plot settings
      cex.axis=1.5,cex.lab=2)
  set.seed(42)                                             # rng
  betas=rmvnorm(n,m$postmean[-1],diag(m$postsd[-1]^2))     # post. distribution
  
  plot(x=0,y=0,col=NA,                                     # empty plot
       xlim=c(floor(min(betas)),ceiling(max(betas))),
       ylim=c(0,ncol(betas)+1),xaxt="n",yaxt="n",ylab="",xlab="",
       main=dvlab)
  axis(1,tick=F)                                           # axis
  segments(x0=0,x1=0,y0=-1,y1=ncol(betas)+1,               # origin
           col="firebrick3",lwd=2,lty=2)
  segments(x0=floor(min(betas)),x1=ceiling(max(betas)),    # lines variables
           y0=1:length(var),lty=3,col="darkgrey")
  
  for(i in 1:ncol(betas)){                                 # add posterior
    points(x=betas[,ncol(betas)-i+1],
           y=runif(n,i-.4,i+.4),
           col=rgb(.1,.3,.5,m$probne0[ncol(betas)-i+1]*.005),pch=18,cex=.5)
    
    segments(x0=mean(betas[,ncol(betas)-i+1]),
             x1=mean(betas[,ncol(betas)-i+1]),
             y0=i-.5,y1=i+.5,col=rgb(1,1,1),lwd=3)
    
    segments(x0=quantile(betas[,ncol(betas)-i+1],.17),
             x1=quantile(betas[,ncol(betas)-i+1],.17),
             y0=i-.5,y1=i+.5,lty=1,col=rgb(1,1,1),lwd=2)
    
    segments(x0=quantile(betas[,ncol(betas)-i+1],.83),
             x1=quantile(betas[,ncol(betas)-i+1],.83),
             y0=i-.5,y1=i+.5,lty=1,col=rgb(1,1,1),lwd=2)
  }
  
  par(xpd=NA)
  cutz<-seq(floor(min(betas)),ceiling(max(betas)),length.out=11)
  for(j in 1:10){
    rect(xleft=cutz[j],ybottom=-ncol(betas)/4,
         xright=cutz[j+1],ytop=-ncol(betas)/5,
         col=rgb(.1,.3,.5,c(1:10/10)[j]),border=NA,cex=2)
  }
  text(x=c(floor(min(betas)),mean(c(floor(min(betas)),ceiling(max(betas)))),
           ceiling(max(betas))),y=-ncol(betas)/3.5,
       labels=c(0,.5,1),cex=1.5)
  axis(2,at=-ncol(betas)/4,labels=expression(P(beta!=0)),
       las=1,tick=F,cex.axis=1.5)
  
}

## FIN